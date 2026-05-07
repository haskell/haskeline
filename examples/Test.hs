{-# LANGUAGE CPP #-}
module Main where

import System.Console.Haskeline
import Options.Applicative
import Data.Monoid ((<>))
#ifndef MINGW
import System.IO (openFile, hClose, IOMode(..))
#endif

{--
Testing the line-input functions and their interaction with ctrl-c signals.

Usage:
  ./Test                                  (line input, default Behavior)
  ./Test chars                            (character input)
  ./Test password                         (no masking)
  ./Test password \*                      (masking with '*')
  ./Test initial                          (initial text in the prompt)

  ./Test --mode useTermHandles            (POSIX only)
  ./Test --mode useTermHandlesWith --term-type vt100
                                          (POSIX only)

The --mode flag selects the haskeline Behavior; positional INPUT_ARG
selects which prompt function to use, and works with any --mode.
--}

data Mode
    = UseTerm                    -- ^ defaultBehavior
    | UseTermHandles             -- ^ useTermHandles on /dev/tty
    | UseTermHandlesWith String  -- ^ useTermHandlesWith TERM on /dev/tty

data InputMode
    = LineInput
    | CharInput
    | PasswordInput (Maybe Char)
    | InitialInput

data Opts = Opts { optMode :: Mode, optInput :: InputMode }

mySettings :: Settings IO
mySettings = defaultSettings {historyFile = Just "myhist"}

main :: IO ()
main = do
    Opts {optMode = m, optInput = im} <- execParser optsInfo
    case m of
        UseTerm ->
            runInputT mySettings (runAction im)
#ifndef MINGW
        UseTermHandles ->
            runWithHandles Nothing im
        UseTermHandlesWith t ->
            runWithHandles (Just t) im
#else
        _ -> error "useTermHandles[With] is not available on Windows"
#endif

runAction :: InputMode -> InputT IO ()
runAction im = withInterrupt $ loop (inputFunc im) 0

inputFunc :: InputMode -> String -> InputT IO (Maybe String)
inputFunc LineInput          = getInputLine
inputFunc CharInput          = fmap (fmap (\c -> [c])) . getInputChar
inputFunc (PasswordInput mc) = getPassword mc
inputFunc InitialInput       = flip getInputLineWithInitial ("left ", "right")

loop :: (String -> InputT IO (Maybe String)) -> Int -> InputT IO ()
loop f n = do
    minput <- handleInterrupt (return (Just "Caught interrupted"))
                $ f (show n ++ ":")
    case minput of
        Nothing     -> return ()
        Just "quit" -> return ()
        Just "q"    -> return ()
        Just s      -> do
            outputStrLn ("line " ++ show n ++ ":" ++ s)
            loop f (n+1)

#ifndef MINGW
-- Drive Haskeline against /dev/tty (the controlling terminal) via the
-- useTermHandles[With] Behaviors.  This still happens to be the controlling
-- tty in our test setup, but it goes through the new code path.
runWithHandles :: Maybe String -> InputMode -> IO ()
runWithHandles mTerm im = do
    input  <- openFile "/dev/tty" ReadMode
    output <- openFile "/dev/tty" WriteMode
    let beh = case mTerm of
            Nothing -> useTermHandles input output
            Just t  -> useTermHandlesWith t input output
    runInputTBehavior beh mySettings (runAction im)
    hClose input
    hClose output
#endif

----------------------------------------------------------------
-- Command-line parsing

optsInfo :: ParserInfo Opts
optsInfo = info (optsP <**> helper)
                (fullDesc <> progDesc "Haskeline test program")

optsP :: Parser Opts
optsP = Opts <$> modeP <*> inputModeP

modeP :: Parser Mode
modeP = mkMode
    <$> strOption
            ( long "mode"
           <> value "useTerm"
           <> showDefault
           <> metavar "MODE"
           <> help "useTerm | useTermHandles | useTermHandlesWith" )
    <*> optional
            (strOption
                ( long "term-type"
               <> metavar "TERM"
               <> help "term type for --mode useTermHandlesWith" ))
  where
    mkMode "useTerm"            _         = UseTerm
    mkMode "useTermHandles"     _         = UseTermHandles
    mkMode "useTermHandlesWith" (Just t)  = UseTermHandlesWith t
    mkMode "useTermHandlesWith" Nothing   =
        error "--mode useTermHandlesWith requires --term-type"
    mkMode other                _         =
        error ("unknown --mode: " ++ other)

inputModeP :: Parser InputMode
inputModeP = mkInput <$> many (argument str (metavar "INPUT_ARG"))
  where
    mkInput []                  = LineInput
    mkInput ["chars"]           = CharInput
    mkInput ["password"]        = PasswordInput Nothing
    mkInput ["password", [c]]   = PasswordInput (Just c)
    mkInput ["initial"]         = InitialInput
    mkInput xs                  =
        error ("unrecognized positional args: " ++ show xs)
