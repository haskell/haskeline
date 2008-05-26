module System.Console.Haskeline(InputT,
                    runInputT,
                    runInputTWithPrefs,
                    getInputLine,
                    Settings(..),
                    defaultSettings,
                    Prefs(..),
                    BellStyle(..),
                    EditMode(..),
                    defaultPrefs,
                    readPrefs,
                    CompletionType(..),
                    -- * Tab completion functions
                    CompletionFunc,
                    Completion(..),
                    completeWord,
                    simpleCompletion,
                    completeFilename,
                    filenameWordBreakChars)
                     where

import System.Console.Haskeline.LineState
import System.Console.Haskeline.Command
{--
import System.Console.Haskeline.Command.Undo
import System.Console.Haskeline.Command.Paste
import System.Console.Haskeline.Command.Completion
--}
import System.Console.Haskeline.Command.History
import System.Console.Haskeline.Vi
import System.Console.Haskeline.Emacs
import System.Console.Haskeline.Settings
import System.Console.Haskeline.Monads
import System.Console.Haskeline.InputT
import System.Console.Haskeline.Command.Completion

import System.IO
import Data.Char (isSpace)
import Control.Monad

#ifdef MINGW
import System.Console.Haskeline.Win32
#else
import System.Console.Haskeline.Draw
import System.Console.Haskeline.Posix
#endif

defaultSettings :: MonadIO m => Settings m
defaultSettings = Settings {complete = completeFilename,
                        historyFile = Nothing,
                        maxHistorySize = Nothing}

-- Note: Without buffering the output, there's a cursor flicker sometimes.
-- We'll keep it buffered, and manually flush the buffer in 
-- repeatTillFinish.
wrapTerminalOps:: MonadIO m => m a -> m a
wrapTerminalOps f = do
    oldInBuf <- liftIO $ hGetBuffering stdin
    oldEcho <- liftIO $ hGetEcho stdout
    let initialize = do 
                        hSetBuffering stdin NoBuffering
                        hSetEcho stdout False
    let reset = do 
                   hSetBuffering stdin oldInBuf
                   hSetEcho stdout oldEcho
    finallyIO (liftIO initialize >> f) reset

getInputLine :: MonadIO m => String -> InputT m (Maybe String)
getInputLine prefix = do
-- TODO: Cache the terminal, actions
    emode <- asks (\prefs -> case editMode prefs of
                    Vi -> viActions
                    Emacs -> emacsCommands)
    wrapTerminalOps $ do
        let ls = emptyIM
        layout <- liftIO getLayout

        result <- runInputCmdT layout $ runDraw 
                    $ withGetEvent $ \getEvent -> 
                        drawLine prefix ls 
                            >> repeatTillFinish getEvent prefix ls emode
        case result of 
            Just line | not (all isSpace line) -> addHistory line
            _ -> return ()
        return result

repeatTillFinish :: forall m s . (MonadIO m, LineState s) 
            => Draw (InputCmdT m) Event -> String -> s -> KeyMap (InputCmdT m) s -> Draw (InputCmdT m) (Maybe String)
repeatTillFinish getEvent prefix = loop
    where 
        -- NOTE: since the functions in this mutually recursive binding group do not have the 
        -- same contexts, we need the -XGADTs flag (or -fglasgow-exts)
        loop :: forall t . LineState t => t -> KeyMap (InputCmdT m) t -> Draw (InputCmdT m) (Maybe String)
        loop s processor = do
                liftIO (hFlush stdout)
                event <- getEvent
                case event of
                    WindowResize newLayout -> 
                        withReposition newLayout (loop s processor)
                    KeyInput k -> case lookupKM processor k of
                        Nothing -> loop s processor
                        Just g -> case g s of
                            Left r -> moveToNextLine s >> return r
                            Right f -> do
                                        KeyAction effect next <- lift f
                                        drawEffect prefix s effect
                                        loop (effectState effect) next
