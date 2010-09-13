{- | 

A rich user interface for line input in command-line programs.  Haskeline is
Unicode-aware and runs both on POSIX-compatible systems and on Windows.  

Users may customize the interface with a @~/.haskeline@ file; see
<http://trac.haskell.org/haskeline/wiki/UserPrefs> for more information.

An example use of this library for a simple read-eval-print loop (REPL) is the
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
                    -- * Interactive sessions
                    -- ** The InputT monad transformer
                    InputT,
                    runInputT,
                    haveTerminalUI,
                    -- ** Behaviors
                    Behavior,
                    runInputTBehavior,
                    defaultBehavior,
                    useFileHandle,
                    useFile,
                    preferTerm,
                    -- * User interaction functions
                    -- ** Reading user input
                    -- $inputfncs
                    getInputLine,
                    getInputChar,
                    getPassword,
                    -- ** Outputting text
                    -- $outputfncs
                    outputStr,
                    outputStrLn,
                    -- * Customization
                    -- ** Settings
                    Settings(..),
                    defaultSettings,
                    setComplete,
                    -- ** User preferences
                    Prefs(),
                    readPrefs,
                    defaultPrefs,
                    runInputTWithPrefs,
                    runInputTBehaviorWithPrefs,
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
import Data.Char (isSpace, isPrint)


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
The following functions enable cross-platform output of text that may contain
Unicode characters.
-}

-- | Write a Unicode string to the user's standard output.
outputStr :: MonadIO m => String -> InputT m ()
outputStr xs = do
    putter <- asks putStrOut
    liftIO $ putter xs

-- | Write a string to the user's standard output, followed by a newline.
outputStrLn :: MonadIO m => String -> InputT m ()
outputStrLn = outputStr . (++ "\n")


{- $inputfncs
The following functions read one line or character of input from the user.

When using terminal-style interaction, these functions return 'Nothing' if the user
pressed @Ctrl-D@ when the input text was empty.

When using file-style interaction, these functions return 'Nothing' if
an @EOF@ was encountered before any characters were read.
-}


{- | Reads one line of input.  The final newline (if any) is removed.  When using terminal-style interaction, this function provides a rich line-editing user interface.

If @'autoAddHistory' == 'True'@ and the line input is nonblank (i.e., is not all
spaces), it will be automatically added to the history.
-}
getInputLine :: forall m . MonadException m => String -- ^ The input prompt
                            -> InputT m (Maybe String)
getInputLine = promptedInput getInputCmdLine $ unMaybeT . getLocaleLine

getInputCmdLine :: MonadException m => TermOps -> String -> InputT m (Maybe String)
getInputCmdLine tops prefix = do
    emode <- asks editMode
    result <- runInputCmdT tops $ case emode of
                Emacs -> runCommandLoop tops prefix emacsCommands emptyIM
                Vi -> evalStateT' emptyViState $
                        runCommandLoop tops prefix viKeyCommands emptyIM
    maybeAddHistory result
    return result

maybeAddHistory :: forall m . Monad m => Maybe String -> InputT m ()
maybeAddHistory result = do
    settings :: Settings m <- ask
    histDupes <- asks historyDuplicates
    case result of
        Just line | autoAddHistory settings && not (all isSpace line) 
            -> let adder = case histDupes of
                        AlwaysAdd -> addHistory
                        IgnoreConsecutive -> addHistoryUnlessConsecutiveDupe
                        IgnoreAll -> addHistoryRemovingAllDupes
               in modify (adder line)
        _ -> return ()

----------

{- | Reads one character of input.  Ignores non-printable characters.

When using terminal-style interaction, the character will be read without waiting
for a newline.

When using file-style interaction, a newline will be read if it is immediately
available after the input character.
-}
getInputChar :: MonadException m => String -- ^ The input prompt
                    -> InputT m (Maybe Char)
getInputChar = promptedInput getInputCmdChar $ \fops -> do
                        c <- getPrintableChar fops
                        maybeReadNewline fops
                        return c

getPrintableChar :: FileOps -> IO (Maybe Char)
getPrintableChar fops = do
    c <- unMaybeT $ getLocaleChar fops
    case fmap isPrint c of
        Just False -> getPrintableChar fops
        _ -> return c
        
getInputCmdChar :: MonadException m => TermOps -> String -> InputT m (Maybe Char)
getInputCmdChar tops prefix = runInputCmdT tops 
        $ runCommandLoop tops prefix acceptOneChar emptyIM

acceptOneChar :: Monad m => KeyCommand m InsertMode (Maybe Char)
acceptOneChar = choiceCmd [useChar $ \c s -> change (insertChar c) s
                                                >> return (Just c)
                          , ctrlChar 'l' +> clearScreenCmd >|>
                                        keyCommand acceptOneChar
                          , ctrlChar 'd' +> failCmd]

----------
-- Passwords

{- | Reads one line of input, without displaying the input while it is being typed.
When using terminal-style interaction, the masking character (if given) will replace each typed character.

When using file-style interaction, this function turns off echoing while reading
the line of input.
-}
 
getPassword :: MonadException m => Maybe Char -- ^ A masking character; e.g., @Just \'*\'@
                            -> String -> InputT m (Maybe String)
getPassword x = promptedInput
                    (\tops prefix -> runInputCmdT tops
                                        $ runCommandLoop tops prefix loop
                                        $ Password [] x)
                    (\fops -> let h_in = inputHandle fops
                              in bracketSet (hGetEcho h_in) (hSetEcho h_in) False
                                  $ unMaybeT $ getLocaleLine fops)
 where
    loop = choiceCmd [ simpleChar '\n' +> finish
                     , simpleKey Backspace +> change deletePasswordChar
                                                >|> loop'
                     , useChar $ \c -> change (addPasswordChar c) >|> loop'
                     , ctrlChar 'd' +> \p -> if null (passwordState p)
                                                then failCmd p
                                                else finish p
                     ]
    loop' = keyCommand loop
                        


-------
-- | Wrapper for input functions.
promptedInput :: MonadIO m => (TermOps -> String -> InputT m a)
                        -> (FileOps -> IO a)
                        -> String -> InputT m a
promptedInput doTerm doFile prompt = do
    -- If other parts of the program have written text, make sure that it
    -- appears before we interact with the user on the terminal.
    liftIO $ hFlush stdout
    rterm <- ask
    case termOps rterm of
        Right fops -> liftIO $ do
                        putStrOut rterm prompt
                        doFile fops
        Left tops -> do
            -- If the prompt contains newlines, print all but the last line.
            let (lastLine,rest) = break (`elem` "\r\n") $ reverse prompt
            outputStr $ reverse rest
            doTerm tops $ reverse lastLine

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



