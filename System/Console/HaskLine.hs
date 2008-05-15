module System.Console.HaskLine where

import System.Console.HaskLine.LineState
import System.Console.HaskLine.Command
import System.Console.HaskLine.Posix
{--
import System.Console.HaskLine.Command.Undo
import System.Console.HaskLine.Command.Paste
import System.Console.HaskLine.Command.Completion
--}
import System.Console.HaskLine.Command.History
import System.Console.HaskLine.Draw
import System.Console.HaskLine.Vi
import System.Console.HaskLine.Emacs
import System.Console.HaskLine.Settings
import System.Console.HaskLine.Monads
import System.Console.HaskLine.HaskLineT
import System.Console.HaskLine.Command.Completion

import System.Console.Terminfo
import System.IO
import Data.Maybe (fromMaybe)
import Data.Char (isSpace)
import Control.Monad


test :: IO ()
test = runHaskLineT defaultSettings $ do
    s <- getHaskLine ">:"
    liftIO (print s)

test2 :: IO ()
test2 = runHaskLineT defaultSettings $ do
    s <- getHaskLine ">:"
    runHaskLineT defaultSettings $ do
        t <- getHaskLine ">:"
        j <- getHaskLine "3:"
        liftIO $ print (t,j)
    q <- getHaskLine "4:"
    liftIO $ print (s,q)

defaultSettings :: MonadIO m => Settings m
defaultSettings = Settings {complete = completeFilename,
                        historyFile = Nothing,
                        maxHistorySize = Nothing}

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



data TermSettings = TermSettings {terminal :: Terminal,
                          actions :: Actions}


makeSettings :: IO TermSettings
makeSettings = do
    t <- setupTermFromEnv
    let Just acts = getCapability t getActions
    return TermSettings {terminal = t, actions = acts}


getHaskLine :: MonadIO m => String -> HaskLineT m (Maybe String)
getHaskLine prefix = do
-- TODO: Cache the terminal, actions
    emode <- asks (\prefs -> case editMode prefs of
                    Vi -> viActions
                    Emacs -> emacsCommands)
    settings <- liftIO makeSettings
    wrapTerminalOps (terminal settings) $ do
        let ls = emptyIM
        layout <- liftIO getLayout

        result <- runHaskLineCmdT
                    $ runDraw (actions settings) (terminal settings) layout
                    $ withGetEvent (terminal settings) $ \getEvent -> 
                        drawLine prefix ls 
                            >> repeatTillFinish getEvent prefix ls emode
        case result of 
            Just line | not (all isSpace line) -> addHistory line
            _ -> return ()
        return result

repeatTillFinish :: forall m s . (MonadIO m, LineState s) 
            => Draw m Event -> String -> s -> KeyMap m s -> Draw m (Maybe String)
repeatTillFinish getEvent prefix = loop
    where 
        -- NOTE: since the functions in this mutually recursive binding group do not have the 
        -- same contexts, we need the -XGADTs flag (or -fglasgow-exts)
        loop :: forall t . LineState t => t -> KeyMap m t -> Draw m (Maybe String)
        loop s processor = do
                        liftIO (hFlush stdout)
                        event <- getEvent
                        case event of
                            WindowResize newLayout -> 
                                actOnResize newLayout s processor
                            KeyInput k -> case lookupKM processor k of
                                    Nothing -> loop s processor
                                    Just f -> do
                                        KeyAction effect next <- lift (f s)
                                        actOnCommand effect s next
                                
        actOnResize newLayout s next
                = withReposition newLayout (loop s next)


        actOnCommand :: forall r t . (LineState r,LineState t) => 
                Effect t -> r -> KeyMap m t -> Draw m (Maybe String)
        actOnCommand Finish s _ = moveToNextLine s >> return (Just (toResult s))
        actOnCommand Fail s _ = moveToNextLine s >> return Nothing
        actOnCommand (Redraw shouldClear t) _ next = do
            if shouldClear
                then clearScreenAndRedraw prefix t
                else redrawLine prefix t
            loop t next
        actOnCommand (Change t) s next = do
            diffLinesBreaking prefix s t
            loop t next
        actOnCommand (PrintLines ls t) s next = do
                            layout <- ask
                            moveToNextLine s
                            output $ mconcat $ map (\l -> text l <#> nl)
                                            $ ls layout
                            drawLine prefix t
                            loop t next
