module Main where

import qualified Data.Map as Map
import LineState
import Command
import WindowSize

import System.Console.Terminfo
import Control.Monad.RWS
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
        layout <- liftIO $ getLayout
        let prefixLen = length prefix
        let pos = TermPos {termRow = prefixLen `div` width layout,
                            termCol = prefixLen `rem` width layout}
        result <- repeatTillFinish settings commands layout 
                        pos (lineState "") 
        liftIO $ runTermOutput (terminal settings) $ nl (actions settings)
        return result

-- todo: make sure >=2
getLayout = fmap (Layout . fromEnum . winCols) getWindowSize


repeatTillFinish :: MonadIO m => 
        Settings -> Commands m -> Layout -> TermPos -> LineState -> m LineState
repeatTillFinish settings commands layout = f
    where f pos ls = do
                k <- liftIO $ getKey (keys settings)
                case Map.lookup k commands of
                    Nothing -> f pos ls
                    Just Finish -> newlines settings layout pos ls >> return ls
                    Just (ChangeCmd g) -> do
                        let newLS = g ls
                        let (_,newPos,act) = runRWS (diffLinesBreaking ls newLS)
                                                layout pos
                        liftIO $ runTermOutput (terminal settings) 
                                $ act (actions settings)
                        f newPos newLS
                
newlines settings layout pos ls = liftIO $ runTermOutput (terminal settings) $ 
                                    mreplicate (lsLinesLeft layout pos ls) nl
                                    $ actions settings
