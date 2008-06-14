module System.Console.Haskeline.InputT where


import System.Console.Haskeline.Command.History
import System.Console.Haskeline.Monads
import System.Console.Haskeline.Settings
import System.Console.Haskeline.Command(Layout)

import System.Directory(getHomeDirectory)
import System.FilePath
import Control.Exception(handle,evaluate)


newtype InputT m a = InputT (StateT History (ReaderT Prefs 
                                (ReaderT (Settings m) m)) a) 
                            deriving (Monad,MonadIO, MonadState History,
                                        MonadReader Prefs)

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
        

-- | Reads prefs from @$HOME/.haskeline@
runInputT :: MonadIO m => Settings m -> InputT m a -> m a
runInputT settings f = do
    prefs <- liftIO readPrefsOrDefault
    runInputTWithPrefs prefs settings f

readPrefsOrDefault :: IO Prefs
readPrefsOrDefault = handle (\_ -> return defaultPrefs) $ do
    home <- getHomeDirectory
    prefs <- readPrefs (home </> ".haskeline")
    evaluate prefs -- make sure this function catches any Read parse errors
    return prefs

