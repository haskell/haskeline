module System.Console.HaskLine.HaskLineT where


import System.Console.HaskLine.Command.History
import System.Console.HaskLine.Monads
import System.Console.HaskLine.Settings

import System.Directory(getHomeDirectory)
import System.FilePath
import Control.Exception(handle,evaluate)


newtype HaskLineT m a= HaskLineT (StateT History (ReaderT Prefs 
                                (ReaderT (Settings m) m)) a) 
                            deriving (Monad,MonadIO, MonadState History,
                                        MonadReader Prefs)

instance MonadTrans HaskLineT where
    lift = HaskLineT . lift . lift . lift
    lift2 f (HaskLineT m) = HaskLineT $ lift2 (lift2 (lift2 f)) m


type HaskLineCmdT m = StateT HistLog (ReaderT Prefs (ReaderT (Settings m) m))

runHaskLineCmdT :: Monad m => HaskLineCmdT m a -> HaskLineT m a
runHaskLineCmdT = HaskLineT . runHistLog

liftCmdT :: Monad m => m a -> HaskLineCmdT m a
liftCmdT = lift  . lift . lift

runHaskLineTWithPrefs :: MonadIO m => Prefs -> Settings m -> HaskLineT m a -> m a
runHaskLineTWithPrefs prefs settings (HaskLineT f) 
    = evalReaderT settings $ evalReaderT prefs 
        $ runHistoryFromFile (historyFile settings) f
        

-- | Reads prefs from $HOME/.haskline
runHaskLineT :: MonadIO m => Settings m -> HaskLineT m a -> m a
runHaskLineT settings f = do
    prefs <- liftIO readPrefsOrDefault
    runHaskLineTWithPrefs prefs settings f

readPrefsOrDefault :: IO Prefs
readPrefsOrDefault = handle (\_ -> return defaultPrefs) $ do
    home <- getHomeDirectory
    prefs <- readPrefs (home </> ".haskline")
    evaluate prefs -- make sure this function catches any Read parse errors
    return prefs

