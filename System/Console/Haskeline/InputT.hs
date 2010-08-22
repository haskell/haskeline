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

import System.Directory(getHomeDirectory)
import System.FilePath
import Control.Applicative
import qualified Control.Monad.State as State
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
newtype InputT m a = InputT {unInputT :: ReaderT RunTerm
                                (StateT History
                                (StateT KillRing (ReaderT Prefs
                                (ReaderT (Settings m) m)))) a}
                            deriving (Monad, MonadIO, MonadException,
                                MonadState History, MonadReader Prefs,
                                MonadReader (Settings m), MonadReader RunTerm)

instance Monad m => Functor (InputT m) where
    fmap = State.liftM

instance Monad m => Applicative (InputT m) where
    pure = return
    (<*>) = State.ap

instance MonadTrans InputT where
    lift = InputT . lift . lift . lift . lift . lift

instance Monad m => State.MonadState History (InputT m) where
    get = get
    put = put

-- for internal use only
type InputCmdT m = StateT Layout (UndoT (StateT HistLog (StateT KillRing
                (ReaderT Prefs (ReaderT (Settings m) m)))))

runInputCmdT :: MonadIO m => TermOps -> InputCmdT m a -> InputT m a
runInputCmdT tops f = InputT $ do
    layout <- liftIO $ getLayout tops
    lift $ runHistLog $ runUndoT $ evalStateT' layout f

instance Monad m => CommandMonad (InputCmdT m) where
    runCompletion lcs = do
        settings <- ask
        lift $ lift $ lift $ lift $ lift $ lift $ complete settings lcs

-- | Run a line-reading application.  Uses 'defaultMode' to determine the
-- interaction behavior.
runInputTWithPrefs :: MonadException m => Prefs -> Settings m -> InputT m a -> m a
runInputTWithPrefs = runInputTForModeWithPrefs defaultMode

-- | Run a line-reading application.  This function should suffice for most applications.
--
-- When using terminal-style interaction, 'Prefs' will be read from the user's @~/.haskeline@
-- file (if present).
-- When using file-style interaction, 'Prefs' are not relevant and will not be read.
--
-- This function is equivalent to @'runInputTForMode' 'defaultMode'@.  It 
-- uses terminal-style interaction if 'stdin' is connected to a terminal and has
-- echoing enabled.  Otherwise (e.g., if 'stdin' is a pipe), it uses file-style interaction.
runInputT :: MonadException m => Settings m -> InputT m a -> m a
runInputT = runInputTForMode defaultMode

-- | Returns 'True' if the current session uses terminal-style interaction.  (See 'Mode'.)
haveTerminalUI :: Monad m => InputT m Bool
haveTerminalUI = asks isTerminalStyle


{- | Haskeline has two types of interaction behaviors:

 * \"Terminal-style\" interaction provides an interactive user interface by connecting
   to the user's terminal (which may be different than 'stdin' or 'stdout').  
 
 * \"File-style\" interaction treats the input as a simple stream of characters, for example
    when reading from a file or pipe.  All output (e.g., the input prompt or @outputStr@)
    is printed to 'stdout'.
 
 A 'Mode' is a method for deciding at run-time which of those behaviors to use.  
 
 For most applications (e.g., a REPL), 'defaultMode' should have the correct behavior.
-}
data Mode = Mode (IO RunTerm)

-- | Create and use a RunTerm, ensuring that it will be closed even if
-- an async exception occurs during the creation or use.
withMode :: MonadException m => Mode -> (RunTerm -> m a) -> m a
withMode (Mode run) f = bracket (liftIO run) (liftIO . closeTerm) f

-- | Run a line-reading application according to the given behavior.
--
-- When using terminal-style interaction, 'Prefs' will be read from the user's @~/.haskeline@
-- file (if present).
-- When using file-style interaction, 'Prefs' are not relevant and will not be read.
runInputTForMode :: MonadException m => Mode -> Settings m -> InputT m a -> m a
runInputTForMode mode settings f = withMode mode $ \run -> do
    prefs <- if isTerminalStyle run
                then liftIO readPrefsFromHome
                else return defaultPrefs
    execInputT prefs settings run f

-- | Run a line-reading application.
runInputTForModeWithPrefs :: MonadException m
    => Mode -> Prefs -> Settings m -> InputT m a -> m a
runInputTForModeWithPrefs mode prefs settings f
    = withMode mode $ flip (execInputT prefs settings) f

-- | Helper function to feed the parameters into an InputT.
execInputT :: MonadException m => Prefs -> Settings m -> RunTerm
                -> InputT m a -> m a
execInputT prefs settings run (InputT f)
    = runReaderT' settings $ runReaderT' prefs
            $ runKillRing
            $ runHistoryFromFile (historyFile settings) (maxHistorySize prefs)
            $ runReaderT f run


-- | Read input from 'stdin'.  
-- Use terminal-style interaction if 'stdin' is connected to
-- a terminal and has echoing enabled.  Otherwise (e.g., if 'stdin' is a pipe), use file-style interaction.
--
-- This mode should suffice for most applications.  
defaultMode :: Mode
defaultMode = Mode defaultRunTerm

-- | Use file-style interaction, reading input from the given 'Handle'.  
fileHandleMode :: Handle -> Mode
fileHandleMode = Mode . fileHandleRunTerm

-- | Use file-style interaction, reading input from the given file.
fileMode :: FilePath -> Mode
fileMode file = Mode $ do
            h <- openBinaryFile file ReadMode
            rt <- fileHandleRunTerm h
            return rt { closeTerm = closeTerm rt >> hClose h}

-- | Use terminal-style interaction whenever possible, even if 'stdin' and/or 'stdout' are not
-- terminals.
--
-- Otherwise, use file-style interaction, reading input from 'stdin'.
preferTermMode :: Mode
preferTermMode = undefined


-- | Read 'Prefs' from @~/.haskeline.@   If there is an error reading the file,
-- the 'defaultPrefs' will be returned.
readPrefsFromHome :: IO Prefs
readPrefsFromHome = handle (\(_::IOException) -> return defaultPrefs) $ do
    home <- getHomeDirectory
    readPrefs (home </> ".haskeline")

