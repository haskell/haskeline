module System.Console.Haskeline(InputT,
                    runInputT,
                    runInputTWithPrefs,
                    getInputLine,
                    Settings(..),
                    defaultSettings,
                    setComplete,
                    Prefs(..),
                    BellStyle(..),
                    EditMode(..),
                    defaultPrefs,
                    readPrefs,
                    CompletionType(..),
                    handleInterrupt,
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
import System.Console.Haskeline.Command.History
import System.Console.Haskeline.Vi
import System.Console.Haskeline.Emacs
import System.Console.Haskeline.Settings
import System.Console.Haskeline.Monads
import System.Console.Haskeline.InputT
import System.Console.Haskeline.Term
import System.Console.Haskeline.Command.Completion

import System.IO
import Data.Char (isSpace)
import Control.Monad
import Control.Exception
import Data.Dynamic

#ifdef MINGW
import System.Console.Haskeline.Backend.Win32 as Win32
#else
import System.Console.Haskeline.Backend.Terminfo as Terminfo
#endif

defaultSettings :: MonadIO m => Settings m
defaultSettings = Settings {complete = completeFilename,
                        historyFile = Nothing,
                        handleSigINT = False}

-- NOTE: If we set stdout to NoBuffering, there can be a flicker effect when many
-- characters are printed at once.  We'll keep it buffered here, and let the Draw
-- monad manually flush outputs that don't print a newline.
wrapTerminalOps:: MonadIO m => m a -> m a
wrapTerminalOps =
    bracketSet (hGetBuffering stdin) (hSetBuffering stdin) NoBuffering
    . bracketSet (hGetBuffering stdout) (hSetBuffering stdout) LineBuffering
    . bracketSet (hGetEcho stdout) (hSetEcho stdout) False


-- TODO: Check at runtime which to use
-- dumb term, and also mingw win32 vs cygwin posix
myRunTerm :: IO (RunTerm MyTerm)

#ifdef MINGW
type MyTerm = Win32.Draw
myRunTerm = return win32Term
#else
type MyTerm = Terminfo.Draw
myRunTerm = do
    mRun <- runTerminfoDraw
    case mRun of 
        Nothing -> error "Couldn't load terminfo and its actions."
        Just run -> return run
#endif



getInputLine, getInputCmdLine :: forall m . MonadIO m => String -> InputT m (Maybe String)
getInputLine prefix = do
    isTerm <- liftIO $ hIsTerminalDevice stdin
    if isTerm
        then getInputCmdLine prefix
        else do
            atEOF <- liftIO $ hIsEOF stdin
            if atEOF
                then return Nothing
                else liftM Just $ liftIO $ hGetLine stdin

getInputCmdLine prefix = do
-- TODO: Cache the terminal, actions
    emode <- asks (\prefs -> case editMode prefs of
                    Vi -> viActions
                    Emacs -> emacsCommands)
    settings :: Settings m <- ask
    wrapTerminalOps $ do
        let ls = emptyIM
        run <- liftIO $ myRunTerm
        layout <- liftIO $ getLayout run
        result <- runInputCmdT layout $ do
                    runTerm run $ withGetEvent run (handleSigINT settings) 
                        $ \getEvent -> do
                            drawLine prefix ls 
                            repeatTillFinish getEvent prefix ls emode
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
                event <- getEvent
                case event of
                    SigInt -> do
                        moveToNextLine s
                        liftIO $ evaluate (throwDyn Interrupt)
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

data Interrupt = Interrupt
                deriving (Show,Typeable,Eq)

handleInterrupt :: MonadIO m => IO a -> m a -> m a
handleInterrupt f = handleIO $ \e -> case dynExceptions e of
                    Just dyn | Just Interrupt <- fromDynamic dyn -> f
                    _ -> throwIO e



drawEffect :: (LineState s, LineState t, MonadIO m) 
    => String -> s -> Effect t -> Draw (InputCmdT m) ()
drawEffect prefix s (Redraw shouldClear t) = if shouldClear
    then clearLayout >> drawLine prefix t
    else clearLine prefix s >> drawLine prefix t
drawEffect prefix s (Change t) = drawLineDiff prefix s t
drawEffect prefix s (PrintLines ls t overwrite) = do
    if overwrite
        then clearLine prefix s
        else moveToNextLine s
    printLines ls
    drawLine prefix t

drawLine :: (LineState s, MonadIO m) => String -> s -> Draw (InputCmdT m) ()
drawLine prefix s = drawLineDiff prefix Cleared s

clearLine :: (LineState s, MonadIO m) => String -> s -> Draw (InputCmdT m) ()
clearLine prefix s = drawLineDiff prefix s Cleared
        
