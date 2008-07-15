module System.Console.Haskeline.InputT where


import System.Console.Haskeline.Command.History
import System.Console.Haskeline.Monads as Monads
import System.Console.Haskeline.Prefs
import System.Console.Haskeline.Command(Layout)
import System.Console.Haskeline.Completion
import System.Console.Haskeline.Backend
import System.Console.Haskeline.Term

import System.Directory(getHomeDirectory)
import System.FilePath

-- | Application-specific customizations to the user interface.
data Settings m = Settings {complete :: CompletionFunc m, -- ^ Custom tab completion
                            historyFile :: Maybe FilePath,
                            handleSigINT :: Bool -- ^ Throw an 'Interrupt'
                            -- exception if the user presses Ctrl-C
                            }

-- | Because 'complete' is the only field of 'Settings' depending on @m@,
-- the expression @defaultSettings {completionFunc = f}@ leads to a type error
-- from being too general.  This function may become unnecessary if another field
-- depending on @m@ is added.

setComplete :: CompletionFunc m -> Settings m -> Settings m
setComplete f s = s {complete = f}


-- | A monad transformer which carries all of the state and settings
-- relevant to a line-reading application.
newtype InputT m a = InputT {unInputT :: ReaderT (RunTerm (InputCmdT m))
                                (StateT History (ReaderT Prefs 
                                (ReaderT (Settings m) m))) a}
                            deriving (Monad,MonadIO, MonadState History,
                                        MonadReader Prefs, MonadReader (Settings m),
                                        MonadReader (RunTerm (InputCmdT m)))


instance MonadTrans InputT where
    lift = InputT . lift . lift . lift . lift

instance MonadException m => MonadException (InputT m) where
    block = InputT . block . unInputT
    unblock = InputT . unblock . unInputT
    catch f h = InputT $ Monads.catch (unInputT f) (unInputT . h)

-- for internal use only
type InputCmdT m = ReaderT Layout (StateT HistLog (ReaderT Prefs (ReaderT (Settings m) m)))

runInputCmdT :: forall m a . MonadIO m => InputCmdT m a -> InputT m a
runInputCmdT f = InputT $ do
    run :: RunTerm (InputCmdT m) <- ask
    layout <- liftIO $ getLayout run
    lift $ runHistLog $ runReaderT' layout f


liftCmdT :: Monad m => m a -> InputCmdT m a
liftCmdT = lift  . lift . lift . lift

runInputTWithPrefs :: MonadException m => Prefs -> Settings m -> InputT m a -> m a
runInputTWithPrefs prefs settings (InputT f) = liftIO myRunTerm >>= \run -> 
    runReaderT' settings $ runReaderT' prefs 
        $ runHistoryFromFile (historyFile settings) (maxHistorySize prefs) 
        $ runReaderT f run
        
-- | Run a line-reading application, reading user 'Prefs' from 
-- @~/.haskeline@
runInputT :: MonadException m => Settings m -> InputT m a -> m a
runInputT settings f = do
    prefs <- liftIO readPrefsFromHome
    runInputTWithPrefs prefs settings f

-- | Read 'Prefs' from @~/.haskeline.@   If there is an error reading the file,
-- the 'defaultPrefs' will be returned.
readPrefsFromHome :: IO Prefs
readPrefsFromHome = handle (\_ -> return defaultPrefs) $ do
    home <- getHomeDirectory
    readPrefs (home </> ".haskeline")

