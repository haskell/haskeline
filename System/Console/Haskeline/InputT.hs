module System.Console.Haskeline.InputT where


import System.Console.Haskeline.Command.History
import System.Console.Haskeline.Monads
import System.Console.Haskeline.Settings
import System.Console.Haskeline.Command(Layout)

import System.Directory(getHomeDirectory)
import System.FilePath
import Control.Exception(handle,evaluate)

-- | A monad transformer which carries all of the state and settings
-- relevant to a line-reading application.
newtype InputT m a = InputT (StateT History (ReaderT Prefs 
                                (ReaderT (Settings m) m)) a) 
                            deriving (Monad,MonadIO, MonadState History,
                                        MonadReader Prefs, MonadReader (Settings m))

instance MonadTrans InputT where
    lift = InputT . lift . lift . lift
    lift2 f (InputT m) = InputT $ lift2 (lift2 (lift2 f)) m

-- for internal use only
type InputCmdT m = ReaderT Layout (StateT HistLog (ReaderT Prefs (ReaderT (Settings m) m)))

runInputCmdT :: Monad m => Layout -> InputCmdT m a -> InputT m a
runInputCmdT layout = InputT . runHistLog . evalReaderT layout

liftCmdT :: Monad m => m a -> InputCmdT m a
liftCmdT = lift  . lift . lift . lift

runInputTWithPrefs :: MonadIO m => Prefs -> Settings m -> InputT m a -> m a
runInputTWithPrefs prefs settings (InputT f) 
    = evalReaderT settings $ evalReaderT prefs 
        $ runHistoryFromFile (historyFile settings) (maxHistorySize prefs) f
        

-- | Run a line-reading application, reading user 'Prefs' from 
-- @$HOME/.haskeline@
runInputT :: MonadIO m => Settings m -> InputT m a -> m a
runInputT settings f = do
    prefs <- liftIO readPrefsOrDefault
    runInputTWithPrefs prefs settings f

-- | Read 'Prefs' from a given file.  If there is an error reading the file, the
-- 'defaultPrefs' will be returned.
readPrefsOrDefault :: IO Prefs
readPrefsOrDefault = handle (\_ -> return defaultPrefs) $ do
    home <- getHomeDirectory
    readPrefs (home </> ".haskeline")

