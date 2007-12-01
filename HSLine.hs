module Main where

import qualified Data.Map as Map
import LineState
import Command

import System.Console.Terminfo
import Control.Monad.Trans
import System.IO
import Control.Exception
import Data.Maybe (fromMaybe)
import Control.Monad

main = do
    t <- setupTermFromEnv
    runHSLine (Settings ">:" t) >>= print


class MonadIO m => MonadIO1 m where
    liftIO1 :: (forall a . IO a -> IO a) -> m a -> m a

instance MonadIO1 IO where
    liftIO1 = id


wrapTerminalOps:: MonadIO1 m => Terminal -> m a -> m a
wrapTerminalOps term = liftIO1 $ \f -> do
    oldInBuf <- hGetBuffering stdin
    oldOutBuf <- hGetBuffering stdout
    oldEcho <- hGetEcho stdout
    bracket_ (do maybeOutput term keypadOn
                 setTerm NoBuffering NoBuffering False)
             (do setTerm oldInBuf oldOutBuf oldEcho
                 maybeOutput term keypadOff)
            f
    where setTerm oldInBuf oldOutBuf oldEcho = do
	         hSetBuffering stdin oldInBuf
		 hSetBuffering stdout oldOutBuf
		 hSetEcho stdout oldEcho


data Settings = Settings {prefix :: String,
                          terminal :: Terminal}

data TermActions = TermActions {refresh :: LineState -> LineState -> IO (),
                                nl :: IO ()}

-- TODO HANDLE FAILURE!
getRefresh :: Settings -> TermActions
getRefresh settings 
    = TermActions {refresh = \ls ls' -> runTermOutput (terminal settings) $
                                            refresh' ls ls',
                   nl = runTermOutput (terminal settings) nl'}
        where Just (refresh',nl') = getCapability (terminal settings) $ 
                                    liftM2 (,)refreshLS newline



maybeOutput :: Terminal -> Capability TermOutput -> IO ()
maybeOutput term cap = runTermOutput term $ 
        fromMaybe mempty (getCapability term cap)




runHSLine :: MonadIO1 m => Settings -> m LineState
runHSLine settings = wrapTerminalOps (terminal settings) $ do
    keySequences <- liftIO $ getKeySequences (terminal settings)
    let commands = simpleCommands -- Is this a setting or not?
    let actions = getRefresh settings
    let initLS = lineState ""
    liftIO $ putStr (prefix settings)
    result <- repeatTillFinish actions (Map.toList keySequences)
         commands (lineState "")
    liftIO $ nl actions
    return result



repeatTillFinish :: MonadIO m => TermActions
                        -> [(String, SKey)]
                        -> Commands m -> LineState -> m LineState
repeatTillFinish actions keySequences commands = f
    where f ls = do
                k <- liftIO $ getKey keySequences
                case Map.lookup k commands of
                    Nothing -> f ls
                    Just Finish -> return ls
                    Just (RefreshLine g) -> do
                        ls' <- g ls
                        liftIO $ refresh actions ls ls'
                        repeatTillFinish actions keySequences commands ls'
                
