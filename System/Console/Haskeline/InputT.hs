module System.Console.Haskeline.InputT where


import System.Console.Haskeline.Command.History
import System.Console.Haskeline.Monads as Monads
import System.Console.Haskeline.Settings
import System.Console.Haskeline.Command(Layout)

import System.Directory(getHomeDirectory)
import System.FilePath

-- | A monad transformer which carries all of the state and settings
-- relevant to a line-reading application.
newtype InputT m a = InputT {unInputT :: StateT History (ReaderT Prefs 
                                (ReaderT (Settings m) m)) a}
                            deriving (Monad,MonadIO, MonadState History,
                                        MonadReader Prefs, MonadReader (Settings m))

instance MonadTrans InputT where
    lift = InputT . lift . lift . lift

instance MonadException m => MonadException (InputT m) where
    block = InputT . block . unInputT
    unblock = InputT . unblock . unInputT
    catch f h = InputT $ Monads.catch (unInputT f) (unInputT . h)

-- for internal use only
type InputCmdT m = ReaderT Layout (StateT HistLog (ReaderT Prefs (ReaderT (Settings m) m)))

runInputCmdT :: Monad m => Layout -> InputCmdT m a -> InputT m a
runInputCmdT layout = InputT . runHistLog . runReaderT' layout

liftCmdT :: Monad m => m a -> InputCmdT m a
liftCmdT = lift  . lift . lift . lift

runInputTWithPrefs :: MonadIO m => Prefs -> Settings m -> InputT m a -> m a
runInputTWithPrefs prefs settings (InputT f) 
    = runReaderT' settings $ runReaderT' prefs 
        $ runHistoryFromFile (historyFile settings) (maxHistorySize prefs) f
        
-- | Run a line-reading application, reading user 'Prefs' from 
-- @~/.haskeline@
runInputT :: MonadIO m => Settings m -> InputT m a -> m a
runInputT settings f = do
    prefs <- liftIO readPrefsFromHome
    runInputTWithPrefs prefs settings f

-- | Read 'Prefs' from @~/.haskeline.@   If there is an error reading the file,
-- the 'defaultPrefs' will be returned.
readPrefsFromHome :: IO Prefs
readPrefsFromHome = handle (\_ -> return defaultPrefs) $ do
    home <- getHomeDirectory
    readPrefs (home </> ".haskeline")

