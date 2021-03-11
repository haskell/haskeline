module System.Console.Haskeline.InputT where


import System.Console.Haskeline.History
import System.Console.Haskeline.Command.History
import System.Console.Haskeline.Command.Undo
import System.Console.Haskeline.Command.KillRing
import System.Console.Haskeline.Monads as Monads
import System.Console.Haskeline.Prefs
import System.Console.Haskeline.Completion
import System.Console.Haskeline.Backend
import System.Console.Haskeline.Term

import Control.Exception (IOException)
import Control.Monad.Catch
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Data.IORef
import System.Directory(getXdgDirectory, XdgDirectory(XdgConfig), doesFileExist, getHomeDirectory)
import System.FilePath
import System.IO

-- | Application-specific customizations to the user interface.
data Settings m = Settings {complete :: CompletionFunc m, -- ^ Custom tab completion.
                            historyFile :: Maybe FilePath, -- ^ Where to read/write the history at the
                                                        -- start and end of each
                                                        -- line input session.
                            autoAddHistory :: Bool -- ^ If 'True', each nonblank line returned by
                                -- @getInputLine@ will be automatically added to the history.

                            }

-- | Because 'complete' is the only field of 'Settings' depending on @m@,
-- the expression @defaultSettings {completionFunc = f}@ leads to a type error
-- from being too general.  This function works around that issue, and may become unnecessary if another field
-- depending on @m@ is added.

setComplete :: CompletionFunc m -> Settings m -> Settings m
setComplete f s = s {complete = f}


-- | A monad transformer which carries all of the state and settings
-- relevant to a line-reading application.
newtype InputT m a = InputT {unInputT :: 
                                ReaderT RunTerm
                                -- Use ReaderT (IO _) vs StateT so that exceptions (e.g., ctrl-c)
                                -- don't cause us to lose the existing state.
                                (ReaderT (IORef History)
                                (ReaderT (IORef KillRing)
                                (ReaderT Prefs
                                (ReaderT (Settings m) m)))) a}
                            deriving (Functor, Applicative, Monad, MonadIO,
                                      MonadThrow, MonadCatch, MonadMask)
                -- NOTE: we're explicitly *not* making InputT an instance of our
                -- internal MonadState/MonadReader classes.  Otherwise haddock
                -- displays those instances to the user, and it makes it seem like
                -- we implement the mtl versions of those classes.

instance MonadTrans InputT where
    lift = InputT . lift . lift . lift . lift . lift

instance ( Fail.MonadFail m ) => Fail.MonadFail (InputT m) where
    fail = lift . Fail.fail

instance ( MonadFix m ) => MonadFix (InputT m) where
    mfix f = InputT (mfix (unInputT . f))

-- | Run an action in the underlying monad, as per 'lift', passing it a runner
-- function which restores the current 'InputT' context. This can be used in
-- the event that we have some function that takes an action in the underlying
-- monad as an argument (such as 'lift', 'hoist', 'forkIO', etc) and we want
-- to compose it with actions in 'InputT'.
withRunInBase :: Monad m =>
    ((forall a . InputT m a -> m a) -> m b) -> InputT m b
withRunInBase inner = InputT $ do
    runTerm <- ask
    history <- ask
    killRing <- ask
    prefs <- ask
    settings <- ask
    lift $ lift $ lift $ lift $ lift $ inner $
        flip runReaderT settings .
        flip runReaderT prefs .
        flip runReaderT killRing .
        flip runReaderT history .
        flip runReaderT runTerm .
        unInputT

-- | Get the current line input history.
getHistory :: MonadIO m => InputT m History
getHistory = InputT get

-- | Set the line input history.
putHistory :: MonadIO m => History -> InputT m ()
putHistory = InputT . put

-- | Change the current line input history.
modifyHistory :: MonadIO m => (History -> History) -> InputT m ()
modifyHistory = InputT . modify

-- for internal use only
type InputCmdT m = StateT Layout (UndoT (StateT HistLog (ReaderT (IORef KillRing)
                        -- HistLog can be just StateT, since its final state
                        -- isn't used outside of InputCmdT.
                (ReaderT Prefs (ReaderT (Settings m) m)))))

runInputCmdT :: MonadIO m => TermOps -> InputCmdT m a -> InputT m a
runInputCmdT tops f = InputT $ do
    layout <- liftIO $ getLayout tops
    history <- get
    lift $ lift $ evalStateT' (histLog history) $ runUndoT $ evalStateT' layout f

instance (MonadIO m, MonadMask m) => CommandMonad (InputCmdT m) where
    runCompletion lcs = do
        settings <- ask
        lift $ lift $ lift $ lift $ lift $ lift $ complete settings lcs

-- | Run a line-reading application.  Uses 'defaultBehavior' to determine the
-- interaction behavior.
runInputTWithPrefs :: (MonadIO m, MonadMask m) => Prefs -> Settings m -> InputT m a -> m a
runInputTWithPrefs = runInputTBehaviorWithPrefs defaultBehavior

-- | Run a line-reading application.  This function should suffice for most applications.
--
-- This function is equivalent to @'runInputTBehavior' 'defaultBehavior'@.  It 
-- uses terminal-style interaction if 'stdin' is connected to a terminal and has
-- echoing enabled.  Otherwise (e.g., if 'stdin' is a pipe), it uses file-style interaction.
--
-- If it uses terminal-style interaction, 'Prefs' will be read from the user's @~/.haskeline@ file
-- (if present).
-- If it uses file-style interaction, 'Prefs' are not relevant and will not be read.
runInputT :: (MonadIO m, MonadMask m) => Settings m -> InputT m a -> m a
runInputT = runInputTBehavior defaultBehavior

-- | Returns 'True' if the current session uses terminal-style interaction.  (See 'Behavior'.)
haveTerminalUI :: Monad m => InputT m Bool
haveTerminalUI = InputT $ asks isTerminalStyle


{- | Haskeline has two ways of interacting with the user:

 * \"Terminal-style\" interaction provides an rich user interface by connecting
   to the user's terminal (which may be different than 'stdin' or 'stdout').  
 
 * \"File-style\" interaction treats the input as a simple stream of characters, for example
    when reading from a file or pipe.  Input functions (e.g., @getInputLine@) print the prompt to 'stdout'.
 
 A 'Behavior' is a method for deciding at run-time which type of interaction to use.  
 
 For most applications (e.g., a REPL), 'defaultBehavior' should have the correct effect.
-}
data Behavior = Behavior (IO RunTerm)

-- | Create and use a RunTerm, ensuring that it will be closed even if
-- an async exception occurs during the creation or use.
withBehavior :: (MonadIO m, MonadMask m) => Behavior -> (RunTerm -> m a) -> m a
withBehavior (Behavior run) f = bracket (liftIO run) (liftIO . closeTerm) f

-- | Run a line-reading application according to the given behavior.
--
-- If it uses terminal-style interaction, 'Prefs' will be read from the
-- user's @~/.haskeline@ file (if present).
-- If it uses file-style interaction, 'Prefs' are not relevant and will not be read.
runInputTBehavior :: (MonadIO m, MonadMask m) => Behavior -> Settings m -> InputT m a -> m a
runInputTBehavior behavior settings f = withBehavior behavior $ \run -> do
    prefs <- if isTerminalStyle run
                then liftIO readUserPrefs
                else return defaultPrefs
    execInputT prefs settings run f

-- | Run a line-reading application.
runInputTBehaviorWithPrefs :: (MonadIO m, MonadMask m)
    => Behavior -> Prefs -> Settings m -> InputT m a -> m a
runInputTBehaviorWithPrefs behavior prefs settings f
    = withBehavior behavior $ flip (execInputT prefs settings) f

-- | Helper function to feed the parameters into an InputT.
execInputT :: (MonadIO m, MonadMask m) => Prefs -> Settings m -> RunTerm
                -> InputT m a -> m a
execInputT prefs settings run (InputT f)
    = runReaderT' settings $ runReaderT' prefs
            $ runKillRing
            $ runHistoryFromFile (historyFile settings) (maxHistorySize prefs)
            $ runReaderT f run

-- | Map a user interaction by modifying the base monad computation.
mapInputT :: (forall b . m b -> m b) -> InputT m a -> InputT m a
mapInputT f = InputT . mapReaderT (mapReaderT (mapReaderT
                                  (mapReaderT (mapReaderT f))))
                    . unInputT

-- | Read input from 'stdin'.  
-- Use terminal-style interaction if 'stdin' is connected to
-- a terminal and has echoing enabled.  Otherwise (e.g., if 'stdin' is a pipe), use
-- file-style interaction.
--
-- This behavior should suffice for most applications.  
defaultBehavior :: Behavior
defaultBehavior = Behavior defaultRunTerm

-- | Use file-style interaction, reading input from the given 'Handle'.  
useFileHandle :: Handle -> Behavior
useFileHandle = Behavior . fileHandleRunTerm

-- | Use file-style interaction, reading input from the given file.
useFile :: FilePath -> Behavior
useFile file = Behavior $ do
            h <- openBinaryFile file ReadMode
            rt <- fileHandleRunTerm h
            return rt { closeTerm = closeTerm rt >> hClose h}

-- | Use terminal-style interaction whenever possible, even if 'stdin' and/or 'stdout' are not
-- terminals.
--
-- If it cannot open the user's terminal, use file-style interaction, reading input from 'stdin'.
preferTerm :: Behavior
preferTerm = Behavior terminalRunTerm

#ifndef MINGW
-- | Use terminal-style interaction on the given input and output handles,
-- taking the terminal type from the @TERM@ environment variable.
--
-- This behavior is for driving Haskeline against a terminal that is not the
-- process's controlling terminal — for example, a serial console, a PTY pair
-- you opened yourself, or a socket-backed TTY.  The caller is responsible for
-- closing @input@ and @output@ after use.  Not available on Windows.
--
-- See 'useTermHandlesWith' to override the terminal type.
useTermHandles :: Handle -> Handle -> Behavior
useTermHandles input output =
    Behavior $ useTermHandlesRunTerm Nothing input output

-- | Like 'useTermHandles', but with the terminal type given explicitly
-- (e.g. @\"xterm-256color\"@ or @\"vt100\"@) instead of read from the @TERM@
-- environment variable.
--
-- The terminal type is only consulted when haskeline is built with terminfo
-- support; in non-terminfo builds it is ignored and a dumb terminal is used.
--
-- ==== __Example: a Haskeline session on a serial port__
--
-- > import System.IO (withFile, IOMode(..))
-- > import System.Console.Haskeline
-- >
-- > serialUI :: FilePath -> IO ()
-- > serialUI devPath =
-- >     withFile devPath ReadMode  $ \input  ->
-- >     withFile devPath WriteMode $ \output ->
-- >         runInputTBehavior (useTermHandlesWith "vt100" input output)
-- >                           defaultSettings
-- >                           loop
-- >   where
-- >     loop = do
-- >         minput <- getInputLine "% "
-- >         case minput of
-- >             Nothing     -> return ()
-- >             Just "quit" -> return ()
-- >             Just s      -> outputStrLn ("got: " ++ s) >> loop
useTermHandlesWith :: String -> Handle -> Handle -> Behavior
useTermHandlesWith termtype input output =
    Behavior $ useTermHandlesRunTerm (Just termtype) input output
#endif

-- | Read 'Prefs' from @$XDG_CONFIG_HOME/haskeline/haskeline@ if present
-- ortherwise @~/.haskeline.@ If there is an error reading the file,
-- the 'defaultPrefs' will be returned.
readUserPrefs :: IO Prefs
readUserPrefs = handle (\(_::IOException) -> return defaultPrefs) $ do
    xdg    <- getXdgDirectory XdgConfig ("haskeline/haskeline")
    exists <- doesFileExist xdg
    home   <- getHomeDirectory
    readPrefs (if exists then xdg else (home </> ".haskeline"))

