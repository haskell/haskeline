module System.Console.HaskLine where

import System.Console.HaskLine.LineState
import System.Console.HaskLine.Command
import System.Console.HaskLine.WindowSize

import System.Console.Terminfo
import qualified Data.Map as Map
import Control.Monad.RWS
import System.IO
import Control.Exception
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent

import System.Posix.Signals.Exts

test = do
    runHSLine ">:" emacsCommands >>= print

emacsCommands = simpleCommands `Map.union` Map.fromList
                    [(KeyChar '\SOH', pureCommand moveToStart)
                    ,(KeyChar '\ENQ', pureCommand moveToEnd)]

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
                          actions :: Actions}


makeSettings :: String -> IO Settings
makeSettings pre = do
    t <- setupTermFromEnv
    let Just acts = getCapability t getActions
    return Settings {prefix = pre, terminal = t, actions = acts}

data EventState m = WindowResize Layout | CommandReceived (Command m) 
                        | Waiting

addNewEvent :: TVar (EventState m) -> EventState m -> STM ()
addNewEvent tv event = do
    old_es <- readTVar tv
    case old_es of
        Waiting -> writeTVar tv event
        _ -> retry

commandLoop :: Terminal -> Commands m -> TVar (EventState m) -> IO ()
commandLoop term commands tv = do
    keySeqs <- fmap Map.toList $ getKeySequences term
    -- Loop until we receive a Finish command
    let loop = do
        k <- getKey keySeqs
        case Map.lookup k commands of
            -- If the key sequence is not set to a command, ignore it.
            Nothing -> loop
            Just cmd -> do
                atomically $ addNewEvent tv (CommandReceived cmd)
                when (not (isFinish cmd)) loop
    loop

-- fork a thread, then kill it after the computation is done
withForked :: IO () -> IO a -> IO a
withForked threadAct f = do
    threadID <- forkIO threadAct
    f `finally` killThread threadID

withWindowHandler :: TVar (EventState m) -> IO a -> IO a
withWindowHandler tv f = do
    let handler = getLayout >>= atomically . addNewEvent tv . WindowResize
    old_handler <- installHandler windowChange (Catch handler) Nothing
    f `finally` installHandler windowChange old_handler Nothing


runHSLine :: MonadIO1 m => String -> Commands m -> m LineState
runHSLine prefix commands = do
    settings <- liftIO (makeSettings prefix) 
    wrapTerminalOps (terminal settings) $ do
        let ls = lineState ""
        layout <- liftIO getLayout
        let pos = posFromLength layout (length prefix)

        tv <- liftIO $ newTVarIO Waiting

        result <- liftIO1 (withWindowHandler tv)
                    $ liftIO1 (withForked 
                                (commandLoop (terminal settings) 
                                    commands tv))
                    
                    $ repeatTillFinish tv settings layout pos ls
        liftIO $ runTermOutput (terminal settings) $ nl (actions settings)
        return result

-- todo: make sure >=2
getLayout = fmap (Layout . fromEnum . winCols) getWindowSize


repeatTillFinish :: MonadIO m => TVar (EventState m) -> Settings
        -> Layout -> TermPos -> LineState -> m LineState
repeatTillFinish tv settings initLayout initPos initLS = do
        liftIO $ putStr (prefix settings)
        loop initLayout initPos initLS
    where 
        loop layout pos ls = join $ liftIO $ atomically $ do
                        event <- readTVar tv
                        case event of
                            Waiting -> retry
                            WindowResize newLayout -> do
                                writeTVar tv Waiting
                                return $ actOnResize layout pos newLayout ls
                            CommandReceived cmd -> do
                                writeTVar tv Waiting
                                return $ actOnCommand cmd layout pos ls
        actOnResize layout pos newLayout ls = do
            let newPos = reposition layout newLayout pos
            loop newLayout newPos ls

        actOnCommand Finish layout pos ls = do
            newlines settings layout pos ls
            return ls
        actOnCommand (ChangeCmd g) layout pos ls = do
            newLS <- g ls
            let (_,newPos,act) = runRWS (diffLinesBreaking ls newLS)
                                        layout pos
            liftIO $ runTermOutput (terminal settings) 
                    $ act (actions settings)
            loop layout newPos newLS
                
newlines settings layout pos ls = liftIO $ runTermOutput (terminal settings) $ 
                                    mreplicate (lsLinesLeft layout pos ls) nl
                                    $ actions settings

