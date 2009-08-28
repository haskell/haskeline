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
import System.Console.Haskeline.RunCommand

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
    emode <- asks editMode
    result <- runInputCmdT tops $ case emode of
                Emacs -> runCommandLoop tops prefix emacsCommands
                Vi -> evalStateT' emptyViState $
                        runCommandLoop tops prefix viKeyCommands
    maybeAddHistory result
    return result

maybeAddHistory :: forall m . Monad m => Maybe String -> InputT m ()
maybeAddHistory result = do
    settings :: Settings m <- ask
    case result of
        Just line | autoAddHistory settings && not (all isSpace line) 
            -> modify (addHistory line)
        _ -> return ()

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

getInputCmdChar :: MonadException m => TermOps -> String -> InputT m (Maybe Char)
getInputCmdChar tops prefix = runInputCmdT tops 
        $ runCommandLoop tops prefix acceptOneChar

acceptOneChar :: Monad m => KeyCommand m InsertMode (Maybe Char)
acceptOneChar = choiceCmd [useChar $ \c s -> change (insertChar c) s
                                                >> return (Just c)
                          , ctrlChar 'd' +> failCmd]

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



