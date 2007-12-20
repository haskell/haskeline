module System.Console.HaskLine where

import System.Console.HaskLine.LineState
import System.Console.HaskLine.Command
import System.Console.HaskLine.Command.Undo
import System.Console.HaskLine.Command.Paste
import System.Console.HaskLine.Command.History
import System.Console.HaskLine.Command.Completion
import System.Console.HaskLine.WindowSize

import System.Console.Terminfo
import qualified Data.Map as Map
import System.IO
import Control.Exception
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM
import Control.Concurrent

import System.Posix.Signals.Exts

import Debug.Trace

test = do
    ls <- runPaste $ runHistory ["foobar", "other", "more"] $ runUndo $ runHSLine ">:" emacsCommands
    print ls

emacsCommands = simpleCommands `Map.union` Map.fromList
                    [(controlKey 'A', pureCommand moveToStart)
                    ,(controlKey 'E', pureCommand moveToEnd)
                    ,(KeySpecial KillLine, undoableKill)
                    ,(KeySpecial KeyUp, historyBack)
                    ,(KeySpecial KeyDown, historyForward)
                    ,(controlKey 'R', pasteCommand) 
                    ,(KeyChar '\t', fileCompletionCmd)
                    ,(controlKey 'L', RedrawLine True)
                    ,(controlKey 'N', RedrawLine False)
                    ]


undoableKill :: (MonadCmd Paste m, MonadCmd Undo m) => Command m
undoableKill = withUndo $ \ls@(LS xs ys) -> do
                    saveForPaste xs
                    return (killLine ls)


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

        tv <- liftIO $ newTVarIO Waiting

        result <- liftIO1 (withWindowHandler tv)
                    $ liftIO1 (withForked 
                                (commandLoop (terminal settings) 
                                    commands tv))
                    
                    $ runDraw (actions settings) (terminal settings) layout
                    $ drawLine prefix ls >> repeatTillFinish tv settings ls
        return result

-- todo: make sure >=2
getLayout = fmap (Layout . fromEnum . winCols) getWindowSize


repeatTillFinish :: forall m . MonadIO m => TVar (EventState m) -> Settings
        -> LineState -> Draw m LineState
repeatTillFinish tv settings = loop
    where 
        loop ls = do
                    liftIO (hFlush stdout)
                    join $ liftIO $ atomically $ do
                        event <- readTVar tv
                        case event of
                            Waiting -> retry
                            WindowResize newLayout -> do
                                writeTVar tv Waiting
                                return $ actOnResize newLayout ls
                            CommandReceived cmd -> do
                                writeTVar tv Waiting
                                return $ actOnCommand cmd ls
        actOnResize newLayout ls
                = withReposition newLayout (loop ls)

        actOnCommand Finish ls = moveToNextLine ls >> return ls
        actOnCommand (RedrawLine shouldClear) ls
            | shouldClear = do clearScreenAndRedraw (prefix settings) ls
                               loop ls

            | otherwise = do redrawLine (prefix settings) ls
                             loop ls
        actOnCommand (Command g) ls = do
            result <- lift (g ls)
            case result of
                Changed newLS -> diffLinesBreaking ls newLS >> loop newLS 
                PrintLines lines newLS -> do 
                            layout <- askLayout
                            moveToNextLine ls 
                            output $ mconcat $ map (\l -> text l `mappend` nl)
                                            $ lines layout
                            drawLine (prefix settings) newLS
                            loop newLS
