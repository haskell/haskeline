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
import System.Console.HaskLine.Draw
import System.Console.HaskLine.Vi
import System.Console.HaskLine.Emacs
import System.Console.HaskLine.Settings
import System.Console.HaskLine.Monads
import System.Console.HaskLine.HaskLineT
import System.Console.HaskLine.Command.Completion

import System.Console.Terminfo
import System.IO
import Control.Exception
import Data.Maybe (fromMaybe)
import Data.Char (isSpace)
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent

import System.Posix.Signals.Exts

test :: IO ()
test = runHaskLineT Settings {complete = completeFilename,
                     historyFile = Just "myhist",
                     maxHistorySize = Nothing} $ do
    s <- getHaskLine ">:"
    liftIO (print s)

-- Note: Without buffering the output, there's a cursor flicker sometimes.
-- We'll keep it buffered, and manually flush the buffer in 
-- repeatTillFinish.
wrapTerminalOps:: MonadIO m => Terminal -> m a -> m a
wrapTerminalOps term f = do
    oldInBuf <- liftIO $ hGetBuffering stdin
    oldEcho <- liftIO $ hGetEcho stdout
    let initialize = do maybeOutput term keypadOn
                        hSetBuffering stdin NoBuffering
                        hSetEcho stdout False
    let reset = do maybeOutput term keypadOff
                   hSetBuffering stdin oldInBuf
                   hSetEcho stdout oldEcho
    finallyIO (liftIO initialize >> f) reset

maybeOutput :: Terminal -> Capability TermOutput -> IO ()
maybeOutput term cap = runTermOutput term $ 
        fromMaybe mempty (getCapability term cap)



data TermSettings = TermSettings {prefix :: String,
                          terminal :: Terminal,
                          actions :: Actions}


makeSettings :: String -> IO TermSettings
makeSettings pre = do
    t <- setupTermFromEnv
    let Just acts = getCapability t getActions
    return TermSettings {prefix = pre, terminal = t, actions = acts}

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
withForked :: MonadIO m => IO () -> m a -> m a
withForked threadAct f = do
    threadID <- liftIO $ forkIO threadAct
    f `finallyIO` killThread threadID

withWindowHandler :: MonadIO m => TVar EventState -> m a -> m a
withWindowHandler tv f = do
    let handler = getLayout >>= atomically . addNewEvent tv . WindowResize
    old_handler <- liftIO $ installHandler windowChange (Catch handler) Nothing
    f `finallyIO` installHandler windowChange old_handler Nothing

getHaskLine :: MonadIO m => String -> HaskLineT m (Maybe String)
getHaskLine prefix = do
-- TODO: Cache the terminal, actions
    emode <- asks (\prefs -> case editMode prefs of
                    Vi -> viActions
                    Emacs -> emacsCommands)
    settings <- liftIO (makeSettings prefix) 
    wrapTerminalOps (terminal settings) $ do
        let ls = emptyIM
        layout <- liftIO getLayout

        tv <- liftIO $ newTVarIO Waiting

        result <- runHaskLineCmdT $ withWindowHandler tv
                    $ withForked (commandLoop (terminal settings) tv)
                    $ runDraw (actions settings) (terminal settings) layout
                    $ drawLine prefix ls >> repeatTillFinish tv settings ls
                                                emode
        case result of 
            Just line | not (all isSpace line) -> addHistory line
            _ -> return ()
        return result

-- todo: make sure >=2
getLayout = fmap mkLayout getWindowSize
    where mkLayout ws = Layout {height = fromEnum (winRows ws),
                                width = fromEnum (winCols ws)}


repeatTillFinish :: forall m s . (MonadIO m, LineState s) 
            => TVar EventState ->TermSettings
                -> s -> KeyMap m s -> Draw m (Maybe String)
repeatTillFinish tv settings = loop
    where 
        -- NOTE: since the functions in this mutually recursive binding group do not have the 
        -- same contexts, we need the -XGADTs flag (or -fglasgow-exts)
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
                                    Just f -> do
                                        KeyAction effect next <- lift (f s)
                                        actOnCommand effect s next
                                
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
                            layout <- ask
                            moveToNextLine s
                            output $ mconcat $ map (\l -> text l <#> nl)
                                            $ ls layout
                            drawLine (prefix settings) t
                            loop t next
