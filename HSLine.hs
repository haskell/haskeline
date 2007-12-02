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
    runHSLine ">:" emacsCommands >>= print

emacsCommands = simpleCommands `Map.union` Map.fromList
                    [(KeyChar '\SOH', ChangeCmd moveToStart)
                    ,(KeyChar '\ENQ', ChangeCmd moveToEnd)]

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

maybeOutput :: Terminal -> Capability TermOutput -> IO ()
maybeOutput term cap = runTermOutput term $ 
        fromMaybe mempty (getCapability term cap)



data Settings = Settings {prefix :: String,
                          terminal :: Terminal,
                          actions :: Actions,
                          keys :: [(String, SKey)]}


makeSettings :: String -> IO Settings
makeSettings pre = do
    t <- setupTermFromEnv
    let Just acts = getCapability t getActions
    keySeqs <- getKeySequences t
    return Settings {prefix = pre, terminal = t, actions = acts,
                     keys = Map.toList keySeqs}





runHSLine :: MonadIO1 m => String -> Commands m -> m LineState
runHSLine prefix commands = do
    settings <- liftIO (makeSettings prefix) 
    wrapTerminalOps (terminal settings) $ do
        let initLS = lineState ""
        liftIO $ putStr prefix
        result <- repeatTillFinish settings commands (lineState "")
        liftIO $ runTermOutput (terminal settings) $ nl (actions settings)
        return result



repeatTillFinish :: MonadIO m => 
        Settings -> Commands m -> LineState -> m LineState
repeatTillFinish settings commands = f
    where f ls = do
                k <- liftIO $ getKey (keys settings)
                case Map.lookup k commands of
                    Nothing -> f ls
                    Just Finish -> return ls
                    Just (ChangeCmd g) -> do
                        let (ls',act) = g ls
                        liftIO $ runTermOutput (terminal settings) 
                            $ act (actions settings)
                        repeatTillFinish settings commands ls'
                
