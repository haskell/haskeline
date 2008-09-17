{- | 

A rich user interface for line input in command-line programs.  Haskeline is
Unicode-aware and runs both on POSIX-compatible systems and on Windows.  

Users may customize the interface with a @~/.haskeline@ file; see the
"System.Console.Haskeline.Prefs" module for more details.

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

If either 'stdin' or 'stdout' is not connected to a terminal (for example, piped from another
process), Haskeline will treat it as a UTF-8-encoded file handle.  

-}


module System.Console.Haskeline(
                    -- * Main functions
                    InputT,
                    runInputT,
                    runInputTWithPrefs,
                    getInputLine,
                    outputStr,
                    outputStrLn,
                    -- * Settings
                    Settings(..),
                    defaultSettings,
                    setComplete,
                    -- * Ctrl-C handling
                    Interrupt(..),
                    handleInterrupt,
                    module System.Console.Haskeline.Completion,
                    module System.Console.Haskeline.Prefs,
                    module System.Console.Haskeline.MonadException)
                     where

import System.Console.Haskeline.LineState
import System.Console.Haskeline.Command
import System.Console.Haskeline.Command.History
import System.Console.Haskeline.Vi
import System.Console.Haskeline.Emacs
import System.Console.Haskeline.Prefs
import System.Console.Haskeline.Monads
import System.Console.Haskeline.MonadException
import System.Console.Haskeline.InputT
import System.Console.Haskeline.Completion
import System.Console.Haskeline.Term

import System.IO
import qualified System.IO.UTF8 as UTF8
import Data.Char (isSpace)
import Control.Monad
import qualified Control.Exception as Exception
import Data.Dynamic




-- | A useful default.  In particular:
--
-- @
-- defaultSettings = Settings {
--           complete = completeFilename,
--           historyFile = Nothing,
--           handleSigINT = False
--           }
-- @
defaultSettings :: MonadIO m => Settings m
defaultSettings = Settings {complete = completeFilename,
                        historyFile = Nothing,
                        handleSigINT = False}

-- | Write a string to the standard output.  Allows cross-platform display of Unicode
-- characters.
outputStr :: MonadIO m => String -> InputT m ()
outputStr xs = do
    putter <- asks putStrOut
    liftIO $ putter xs

-- | Write a string to the standard output, followed by a newline.  Allows
-- cross-platform display of Unicode characters.
outputStrLn :: MonadIO m => String -> InputT m ()
outputStrLn xs = outputStr (xs++"\n")

{- | Read one line of input.  The final newline (if any) is removed.

If 'stdin' is connected to a terminal, 'getInputLine' provides a rich line-editing
user interface.  It returns 'Nothing' if the user presses @Ctrl-D@ when the input
text is empty.  All user interaction, including display of the input prompt, will occur
on the user's output terminal (which may differ from 'stdout').

If 'stdin' is not connected to a terminal, 'getInputLine' reads one line of input, and
prints the prompt and input to 'stdout'. It returns 'Nothing'  if an @EOF@ is
encountered before any characters are read.
-}
getInputLine :: forall m . MonadException m => String -- ^ The input prompt
                            -> InputT m (Maybe String)
getInputLine prefix = do
    rterm <- ask
    case termOps rterm of
        Nothing -> simpleFileLoop prefix rterm
        Just tops -> getInputCmdLine tops prefix

getInputCmdLine :: forall m . MonadException m => TermOps -> String -> InputT m (Maybe String)
getInputCmdLine tops prefix = do
    -- TODO: Cache the actions
    emode <- asks (\prefs -> case editMode prefs of
                    Vi -> viActions
                    Emacs -> emacsCommands)
    settings :: Settings m <- ask
    let ls = emptyIM
    result <- runInputCmdT tops $ flip (runTerm tops) (handleSigINT settings)
                        $ \getEvent -> do
                            drawLine prefix ls 
                            repeatTillFinish getEvent prefix ls emode
    case result of 
        Just line | not (all isSpace line) -> addHistory line
        _ -> return ()
    return result

repeatTillFinish :: forall m s d 
    . (MonadTrans d, Term (d m), MonadIO m, LineState s, MonadReader Prefs m)
            => d m Event -> String -> s -> KeyMap m s 
            -> d m (Maybe String)
repeatTillFinish getEvent prefix = loop
    where 
        -- NOTE: since the functions in this mutually recursive binding group do not have the 
        -- same contexts, we need the -XGADTs flag (or -fglasgow-exts)
        loop :: forall t . LineState t => t -> KeyMap m t -> d m (Maybe String)
        loop s processor = do
                event <- getEvent
                case event of
                    SigInt -> do
                        moveToNextLine s
                        throwInterrupt
                    WindowResize newLayout -> 
                        withReposition newLayout (loop s processor)
                    KeyInput k -> case lookupKM processor k of
                        Nothing -> actBell >> loop s processor
                        Just g -> case g s of
                            Left r -> moveToNextLine s >> return r
                            Right f -> do
                                        KeyAction effect next <- lift f
                                        drawEffect prefix s effect
                                        loop (effectState effect) next

{-- 
When stdin is not a console, just read in one line of input.

NOTE: this behavior "breaks" when we run for example "cat | Test":
Printing the input to stdout is redundant because cat already echoes what the user has
typed.
Given the tradeoffs of all of the different ways of piping to/from stdin/stdout,
I think it's fine because this case seems least likely to occur in practice.
-}

simpleFileLoop :: MonadIO m => String -> RunTerm -> m (Maybe String)
simpleFileLoop prefix rterm = liftIO $ do
    putStrOut rterm prefix
    atEOF <- hIsEOF stdin
    if atEOF
        then return Nothing
        else do
                l <- UTF8.getLine
                putStrOut rterm (l++"\n")
                return (Just l)

{-- 
Note why it is necessary to integrate ctrl-c handling with this module:
if the user is in the middle of a few wrapped lines, we want to clean up
by moving the cursor to the start of the following line.
--}

data Interrupt = Interrupt
                deriving (Show,Typeable,Eq)

-- | Catch and handle an exception of type 'Interrupt'.
handleInterrupt :: MonadException m => m a 
                        -- ^ Handler to run if Ctrl-C is pressed
                     -> m a -- ^ Computation to run
                     -> m a
handleInterrupt f = handle $ \e -> case Exception.dynExceptions e of
                    Just dyn | Just Interrupt <- fromDynamic dyn -> f
                    _ -> throwIO e

throwInterrupt :: MonadIO m => m a
throwInterrupt = liftIO $ Exception.evaluate $ Exception.throwDyn Interrupt


drawEffect :: (LineState s, LineState t, Term (d m), 
                MonadTrans d, MonadReader Prefs m) 
    => String -> s -> Effect t -> d m ()
drawEffect prefix s (Redraw shouldClear t) = if shouldClear
    then clearLayout >> drawLine prefix t
    else clearLine prefix s >> drawLine prefix t
drawEffect prefix s (Change t) = drawLineDiff prefix s t
drawEffect prefix s (PrintLines ls t) = do
    if isTemporary s
        then clearLine prefix s
        else moveToNextLine s
    printLines ls
    drawLine prefix t
drawEffect prefix s (RingBell t) = drawLineDiff prefix s t >> actBell

drawLine :: (LineState s, Term m) => String -> s -> m ()
drawLine prefix s = drawLineDiff prefix Cleared s

clearLine :: (LineState s, Term m) => String -> s -> m ()
clearLine prefix s = drawLineDiff prefix s Cleared
        
actBell :: (Term (d m), MonadTrans d, MonadReader Prefs m) => d m ()
actBell = do
    style <- lift (asks bellStyle)
    case style of
        NoBell -> return ()
        VisualBell -> ringBell False
        AudibleBell -> ringBell True
