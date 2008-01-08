module System.Console.HaskLine where

import System.Console.HaskLine.LineState
import System.Console.HaskLine.Command
{--
import System.Console.HaskLine.Command.Undo
import System.Console.HaskLine.Command.Paste
import System.Console.HaskLine.Command.Completion
--}
import System.Console.HaskLine.Command.History
import System.Console.HaskLine.WindowSize
import System.Console.HaskLine.Modes
import System.Console.HaskLine.Vi
import System.Console.HaskLine.Emacs

import System.Console.Terminfo
import System.IO
import Control.Exception
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM
import Control.Concurrent

import System.Posix.Signals.Exts

test :: IO ()
test = do
    s <- runHistory ["foobar", "other", "more"] $ runHSLine ">:" emacsCommands
    print s

-- Note: Without buffering the output, there's a cursor flicker sometimes.
-- We'll keep it buffered, and manually flush the buffer in 
-- repeatTillFinish.
wrapTerminalOps:: MonadIO1 m => Terminal -> m a -> m a
wrapTerminalOps term = liftIO1 $ \f -> do
    oldInBuf <- hGetBuffering stdin
    oldEcho <- hGetEcho stdout
    bracket_ (do maybeOutput term keypadOn
                 hSetBuffering stdin NoBuffering
                 hSetEcho stdout False)
             (do maybeOutput term keypadOff
                 hSetBuffering stdin oldInBuf
                 hSetEcho stdout oldEcho)
            f

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

data EventState = WindowResize Layout | KeyInput Key
                        | Waiting

addNewEvent :: TVar EventState -> EventState -> STM ()
addNewEvent tv event = do
    old_es <- readTVar tv
    case old_es of
        Waiting -> writeTVar tv event
        _ -> retry

commandLoop :: Terminal -> TVar EventState -> IO ()
commandLoop term tv = do
    keySeqs <- getKeySequences term
    let loop = do
        k <- getKey keySeqs
        atomically $ addNewEvent tv (KeyInput k) 
        loop
    loop

-- fork a thread, then kill it after the computation is done
withForked :: IO () -> IO a -> IO a
withForked threadAct f = do
    threadID <- forkIO threadAct
    f `finally` killThread threadID

withWindowHandler :: TVar EventState -> IO a -> IO a
withWindowHandler tv f = do
    let handler = getLayout >>= atomically . addNewEvent tv . WindowResize
    old_handler <- installHandler windowChange (Catch handler) Nothing
    f `finally` installHandler windowChange old_handler Nothing


runHSLine :: MonadIO1 m => String -> KeyMap m InsertMode -> m (Maybe String)
runHSLine prefix process = do
    settings <- liftIO (makeSettings prefix) 
    wrapTerminalOps (terminal settings) $ do
        let ls = emptyIM
        layout <- liftIO getLayout

        tv <- liftIO $ newTVarIO Waiting

        result <- liftIO1 (withWindowHandler tv)
                    $ liftIO1 (withForked 
                                (commandLoop (terminal settings) tv))
                    
                    $ runDraw (actions settings) (terminal settings) layout
                    $ drawLine prefix ls >> repeatTillFinish tv settings ls
                                                process
        return result

-- todo: make sure >=2
getLayout = fmap mkLayout getWindowSize
    where mkLayout ws = Layout {height = fromEnum (winRows ws),
                                width = fromEnum (winCols ws)}


repeatTillFinish :: forall m s . (MonadIO m, LineState s) 
            => TVar EventState -> Settings
                -> s -> KeyMap m s -> Draw m (Maybe String)
repeatTillFinish tv settings = loop
    where 
        loop :: forall m s . (MonadIO m, LineState s) => 
                s -> KeyMap m s -> Draw m (Maybe String)
        loop s processor = do
                    liftIO (hFlush stdout)
                    join $ liftIO $ atomically $ do
                        event <- readTVar tv
                        case event of
                            Waiting -> retry
                            WindowResize newLayout -> do
                                writeTVar tv Waiting
                                return $ actOnResize newLayout s processor
                            KeyInput k -> do
                                writeTVar tv Waiting 
                                return $ case lookupKM processor k of
                                    Nothing -> loop s processor
                                    Just (KeyAction f next) -> do
                                                    cmd <- lift (f s)
                                                    actOnCommand cmd s next
                                
        actOnResize newLayout s next
                = withReposition newLayout (loop s next)


        actOnCommand :: forall m s t . (MonadIO m, LineState s, LineState t) => 
                Effect t -> 
                s -> KeyMap m t -> Draw m (Maybe String)
        actOnCommand Finish s _ = moveToNextLine s >> return (Just (toResult s))
        actOnCommand Fail _ _ = return Nothing
        actOnCommand (Redraw shouldClear t) _ next = do
            if shouldClear
                then clearScreenAndRedraw (prefix settings) t
                else redrawLine (prefix settings) t
            loop t next
        actOnCommand (Change t) s next = do
            diffLinesBreaking (prefix settings) s t
            loop t next
        actOnCommand (PrintLines ls t) s next = do
                            layout <- askLayout
                            moveToNextLine s
                            output $ mconcat $ map (\l -> text l <#> nl)
                                            $ ls layout
                            drawLine (prefix settings) t
                            loop t next
