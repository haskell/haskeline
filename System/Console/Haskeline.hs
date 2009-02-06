{- | 

A rich user interface for line input in command-line programs.  Haskeline is
Unicode-aware and runs both on POSIX-compatible systems and on Windows.  

Users may customize the interface with a @~/.haskeline@ file; see
<http://trac.haskell.org/haskeline/wiki/UserPrefs> for more information.

An example use of this library for a simple read-eval-print loop is the
following:

> import System.Console.Haskeline
> 
> main :: IO ()
> main = runInputT defaultSettings loop
>    where 
>        loop :: InputT IO ()
>        loop = do
>            minput <- getInputLine "% "
>            case minput of
>                Nothing -> return ()
>                Just "quit" -> return ()
>                Just input -> do outputStrLn $ "Input was: " ++ input
>                                 loop

-}


module System.Console.Haskeline(
                    -- * Main functions
                    -- ** The InputT monad transformer
                    InputT,
                    runInputT,
                    runInputTWithPrefs,
                    -- ** Reading user input
                    -- $inputfncs
                    getInputLine,
                    getInputChar,
                    -- ** Outputting text
                    -- $outputfncs
                    outputStr,
                    outputStrLn,
                    -- * Settings
                    Settings(..),
                    defaultSettings,
                    setComplete,
                    -- * User preferences
                    Prefs(),
                    readPrefs,
                    defaultPrefs,
                    -- * Ctrl-C handling
                    -- $ctrlc
                    Interrupt(..),
                    withInterrupt,
                    handleInterrupt,
                    module System.Console.Haskeline.Completion,
                    module System.Console.Haskeline.MonadException)
                     where

import System.Console.Haskeline.LineState
import System.Console.Haskeline.Command
import System.Console.Haskeline.Vi
import System.Console.Haskeline.Emacs
import System.Console.Haskeline.Prefs
import System.Console.Haskeline.History
import System.Console.Haskeline.Monads
import System.Console.Haskeline.MonadException
import System.Console.Haskeline.InputT
import System.Console.Haskeline.Completion
import System.Console.Haskeline.Term
import System.Console.Haskeline.Key

import System.IO
import Data.Char (isSpace)
import Control.Monad
import Data.Char(isPrint)
import qualified Data.ByteString.Char8 as B
import System.IO.Error (isEOFError)


-- | A useful default.  In particular:
--
-- @
-- defaultSettings = Settings {
--           complete = completeFilename,
--           historyFile = Nothing,
--           autoAddHistory = True
--           }
-- @
defaultSettings :: MonadIO m => Settings m
defaultSettings = Settings {complete = completeFilename,
                        historyFile = Nothing,
                        autoAddHistory = True}

{- $outputfncs
The following functions allow cross-platform output of text that may contain
Unicode characters.
-}

-- | Write a string to the standard output.
outputStr :: MonadIO m => String -> InputT m ()
outputStr xs = do
    putter <- asks putStrOut
    liftIO $ putter xs

-- | Write a string to the standard output, followed by a newline.
outputStrLn :: MonadIO m => String -> InputT m ()
outputStrLn xs = outputStr (xs++"\n")


{- $inputfncs
The following functions read one line or character of input from the user.

If 'stdin' is connected to a terminal, then these functions perform all user interaction,
including display of the prompt text, on the user's output terminal (which may differ from
'stdout').
They return 'Nothing' if the user pressed @Ctrl-D@ when the
input text was empty.

If 'stdin' is not connected to a terminal or does not have echoing enabled,
then these functions print the prompt to 'stdout',
and they return 'Nothing' if an @EOF@ was encountered before any characters were read.
-}


{- | Reads one line of input.  The final newline (if any) is removed.  Provides a rich
line-editing user interface if 'stdin' is a terminal.

If @'autoAddHistory' == 'True'@ and the line input is nonblank (i.e., is not all
spaces), it will be automatically added to the history.
-}
getInputLine :: forall m . MonadException m => String -- ^ The input prompt
                            -> InputT m (Maybe String)
getInputLine prefix = do
    -- If other parts of the program have written text, make sure that it 
    -- appears before we interact with the user on the terminal.
    liftIO $ hFlush stdout
    rterm <- ask
    echo <- liftIO $ hGetEcho stdin
    case termOps rterm of
        Just tops | echo -> getInputCmdLine tops prefix
        _ -> simpleFileLoop prefix rterm

getInputCmdLine :: MonadException m => TermOps -> String -> InputT m (Maybe String)
getInputCmdLine tops prefix = do
    -- Load the necessary settings/prefs
    -- TODO: Cache the actions
    emode <- asks (\prefs -> case editMode prefs of
                    Vi -> viActions
                    Emacs -> emacsCommands)
    -- Run the main event processing loop
    result <- runInputCmdT tops $ runTerm tops
                    $ \getEvent -> do
                            let ls = emptyIM
                            drawLine prefix ls 
                            repeatTillFinish tops getEvent prefix ls emode
    maybeAddHistory result
    return result

maybeAddHistory :: forall m . Monad m => Maybe String -> InputT m ()
maybeAddHistory result = do
    settings :: Settings m <- ask
    case result of
        Just line | autoAddHistory settings && not (all isSpace line) 
            -> modify (addHistory line)
        _ -> return ()

repeatTillFinish :: forall m s d 
    . (MonadTrans d, Term (d m), LineState s, MonadReader Prefs m)
            => TermOps -> d m Event -> String -> s -> KeyMap m s 
            -> d m (Maybe String)
repeatTillFinish tops getEvent prefix = loop []
    where 
        loop :: forall t . LineState t
                    => [Key] -> t -> KeyMap m t -> d m (Maybe String)
        loop [] s processor = do
                event <- handle (\(e::SomeException) -> movePast prefix s >> throwIO e) getEvent
                case event of
                    ErrorEvent e -> movePast prefix s >> throwIO e
                    WindowResize -> withReposition tops prefix s $ loop [] s processor
                    KeyInput k -> do
                        ks <- lift $ asks $ lookupKeyBinding k
                        loop ks s processor
        loop (k:ks) s processor = case lookupKM processor k of
                        Nothing -> actBell >> loop [] s processor
                        Just g -> case g s of
                            Left r -> movePast prefix s >> return r
                            Right f -> do
                                        KeyAction effect next <- lift f
                                        drawEffect prefix s effect
                                        loop ks (effectState effect) next

simpleFileLoop :: MonadIO m => String -> RunTerm -> m (Maybe String)
simpleFileLoop prefix rterm = liftIO $ do
    putStrOut rterm prefix
    atEOF <- hIsEOF stdin
    if atEOF
        then return Nothing
        else do
            -- It's more efficient to use B.getLine, but that function throws an
            -- error if stdin is set to NoBuffering.
            buff <- hGetBuffering stdin
            line <- case buff of
                        NoBuffering -> fmap B.pack System.IO.getLine
                        _ -> B.getLine
            fmap Just $ decodeForTerm rterm line

drawEffect :: (LineState s, LineState t, Term (d m), 
                MonadTrans d, MonadReader Prefs m) 
    => String -> s -> Effect t -> d m ()
drawEffect prefix s (Redraw shouldClear t) = if shouldClear
    then clearLayout >> drawLine prefix t
    else clearLine prefix s >> drawLine prefix t
drawEffect prefix s (Change t) = drawLineStateDiff prefix s t
drawEffect prefix s (PrintLines ls t) = do
    if isTemporary s
        then clearLine prefix s
        else movePast prefix s
    printLines ls
    drawLine prefix t
drawEffect prefix s (RingBell t) = drawLineStateDiff prefix s t >> actBell

drawLine :: (LineState s, Term m) => String -> s -> m ()
drawLine prefix s = drawLineStateDiff prefix Cleared s

drawLineStateDiff :: (LineState s, LineState t, Term m) 
                        => String -> s -> t -> m ()
drawLineStateDiff prefix s t = drawLineDiff (lineChars prefix s) 
                                        (lineChars prefix t)

clearLine :: (LineState s, Term m) => String -> s -> m ()
clearLine prefix s = drawLineStateDiff prefix s Cleared
        
actBell :: (Term (d m), MonadTrans d, MonadReader Prefs m) => d m ()
actBell = do
    style <- lift (asks bellStyle)
    case style of
        NoBell -> return ()
        VisualBell -> ringBell False
        AudibleBell -> ringBell True

movePast :: (LineState s, Term m) => String -> s -> m ()
movePast prefix s = moveToNextLine (lineChars prefix s)

withReposition :: (LineState s, Term m) => TermOps -> String -> s -> m a -> m a
withReposition tops prefix s f = do
    oldLayout <- ask
    newLayout <- liftIO $ getLayout tops
    if oldLayout == newLayout
        then f
        else local newLayout $ do
                reposition oldLayout (lineChars prefix s)
                f
----------

{- | Reads one character of input.  Ignores non-printable characters.

If stdin is a terminal, the character will be read without waiting for a newline.

If stdin is not a terminal, a newline will be read if it is immediately
available after the input character.
-}
getInputChar :: MonadException m => String -- ^ The input prompt
                    -> InputT m (Maybe Char)
getInputChar prefix = do
    liftIO $ hFlush stdout
    rterm <- ask
    echo <- liftIO $ hGetEcho stdin
    case termOps rterm of
        Just tops | echo -> getInputCmdChar tops prefix
        _ -> simpleFileChar prefix rterm

simpleFileChar :: MonadIO m => String -> RunTerm -> m (Maybe Char)
simpleFileChar prefix rterm = liftIO $ do
    putStrOut rterm prefix
    c <- getPrintableChar
    maybeReadNewline
    return c
  where
    getPrintableChar = returnOnEOF Nothing $ do
                c <- getLocaleChar rterm
                if isPrint c
                    then return (Just c)
                    else getPrintableChar

-- If another character is immediately available, and it is a newline, consume it.
--
-- Note that in ghc-6.8.3 and earlier, hReady returns False at an EOF,
-- whereas in ghc-6.10.1 and later it throws an exception.  (GHC trac #1063).
-- This code handles both of those cases.
--
-- Also note that on Windows with ghc<6.10, hReady may not behave correctly (#1198)
-- The net result is that this might cause
-- But this function will generally only be used when reading buffered input
-- (since stdin isn't a terminal), so it should probably be OK.
maybeReadNewline :: IO ()
maybeReadNewline = returnOnEOF () $ do
    ready <- hReady stdin
    when ready $ do
        c <- hLookAhead stdin
        when (c == '\n') $ getChar >> return ()

returnOnEOF :: a -> IO a -> IO a
returnOnEOF x = handle $ \e -> if isEOFError e
                                then return x
                                else throwIO e

-- TODO: it might be possible to unify this function with getInputCmdLine,
-- maybe by calling repeatTillFinish here...
-- It shouldn't be too hard to make Commands parametrized over a return
-- value (which would be Maybe Char in this case).
-- My primary obstacle is that there's currently no way to have a
-- single character input cause a character to be printed and then
-- immediately exit without waiting for Return to be pressed.
getInputCmdChar :: MonadException m => TermOps -> String -> InputT m (Maybe Char)
getInputCmdChar tops prefix = runInputCmdT tops $ runTerm tops $ \getEvent -> do
                                                drawLine prefix emptyIM
                                                loop getEvent
    where
        s = emptyIM
        loop :: Term m => m Event -> m (Maybe Char)
        loop getEvent = do
            event <- handle (\(e::SomeException) -> movePast prefix emptyIM >> throwIO e) getEvent
            case event of
                KeyInput (Key m (KeyChar c))
                    | m /= noModifier -> loop getEvent
                    | c == '\EOT'     -> movePast prefix s >> return Nothing
                    | isPrint c -> do
                            let s' = insertChar c s
                            drawLineStateDiff prefix s s'
                            movePast prefix s'
                            return (Just c)
                WindowResize -> withReposition tops prefix emptyIM $ loop getEvent
                _ -> loop getEvent


------------
-- Interrupt

{- $ctrlc
The following functions provide portable handling of Ctrl-C events.  

These functions are not necessary on GHC version 6.10 or later, which
processes Ctrl-C events as exceptions by default.
-}

-- | If Ctrl-C is pressed during the given computation, throw an exception of type 
-- 'Interrupt'.
withInterrupt :: MonadException m => InputT m a -> InputT m a
withInterrupt f = do
    rterm <- ask
    wrapInterrupt rterm f

-- | Catch and handle an exception of type 'Interrupt'.
handleInterrupt :: MonadException m => m a 
                        -- ^ Handler to run if Ctrl-C is pressed
                     -> m a -- ^ Computation to run
                     -> m a
handleInterrupt f = handleDyn $ \Interrupt -> f



