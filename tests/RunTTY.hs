{-# LANGUAGE RecordWildCards #-}
-- This module provides an interface for running terminal-using programs
-- without the presence of a terminal.
-- It uses the "script" command to run a program and capture its output.
module RunTTY (Invocation(..), 
            runInvocation, 
            assertInvocation,
            testI,
            setLang,
            setTerm,
            setLatin1,
            setUTF8
            ) where

import Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Posix.Env.ByteString hiding (setEnv)
import System.Process
import System.Directory
import System.Timeout
import Control.Concurrent.Async
import Control.Concurrent
import System.IO
import Test.HUnit
import Control.Monad (unless)


data Invocation = Invocation {
            prog :: FilePath
            , progArgs :: [String]
            , runInTTY :: Bool
            , environment :: [(String,String)]
            }

setEnv :: String -> String -> Invocation -> Invocation
setEnv var val Invocation {..} = Invocation{
        environment = (var,val) : Prelude.filter ((/=var).fst) environment 
        ,..
        }

setLang = setEnv "LANG"
setTerm = setEnv "TERM"


setUTF8 = setLang "en_US.UTF-8"
setLatin1 = setLang "en_US.ISO8859-1"


runInvocation :: Invocation 
        -> [B.ByteString] -- Input chunks.  (We pause after each chunk to
                        -- simulate real user input and prevent Haskeline
                        -- from coalescing the changes.)
        -> IO B.ByteString
runInvocation Invocation {..} input = do
    tempDir <- getTemporaryDirectory
    (fTemp,hTemp) <- openTempFile tempDir "input.txt"
    hClose hTemp
    let p
          | not runInTTY = proc prog progArgs
          | otherwise = proc "script" $
                        [ "-q" -- no start/stop status
                        , "-k" -- include user input
                        , "-t", "0" -- flush after every character I/O
                        , fTemp
                        , prog
                        ] ++ progArgs
    (Just inH, Just outH, Nothing, ph)
        <- createProcess p
                            { env = Just environment
                            , std_in = CreatePipe
                            , std_out = CreatePipe
                            , std_err = Inherit
                            }
    readOut <- async $ B.hGetContents outH
    mapM_ (\i -> B.hPutStr inH i >> hFlush inH >> threadDelay 20000) input
    hClose inH
    -- if the process is paused, wait 1/5 of a second before forcing it
    -- to close.
    race (waitForProcess ph)
        (threadDelay 100000
            >> terminateProcess ph >> waitForProcess ph)
    output <- wait readOut
    tempContents <- B.readFile fTemp
    removeFile fTemp
    return $ if runInTTY then tempContents else output
    


assertInvocation :: Invocation -> [B.ByteString] -> [B.ByteString]
                    -> Assertion
assertInvocation i input output = do
    let expectedOutput = if runInTTY i
                            then interleave input output
                            else B.concat output
    actualOutput <- runInvocation i input
    assertSame ((if runInTTY i then fixInput else id) expectedOutput)
        (fixOutput actualOutput)

interleave (x:xs) (y:ys) = x `B.append` y `B.append` interleave xs ys
interleave xs [] = B.concat xs
interleave [] ys = B.concat ys

-- script expands LF -> CRLF (like a normal terminal would)
-- so we'll do the same for our inputs/outputs
fixInput = B.concatMap 
            $ \c -> if c == 10 then B.pack  [13,10] else B.singleton c

-- script turns "\ESC" from input into "^["
-- so we'll normalize any "^[" into "\ESC"
fixOutput = BC.pack . loop . BC.unpack
  where
    loop ('^':'[':rest) = '\ESC':loop rest
    loop (c:cs) = c : loop cs
    loop [] = []

assertSame :: B.ByteString -> B.ByteString -> Assertion
assertSame expected actual = do
    let (same,expected',actual') = commonPrefix expected actual
    unless (B.null expected'  && B.null actual') $ assertFailure
        $ "With common prefix " ++ show same ++ "\n"
        ++ "  expected: " ++ show expected' ++ "\n"
        ++ "   but got: " ++ show actual'
        ++ if normalizeErrs expected' == normalizeErrs actual'
            then "\n  (Same except for error chars)"
            else ""


commonPrefix :: B.ByteString -> B.ByteString
    -> (B.ByteString, B.ByteString,B.ByteString)
commonPrefix xs ys = loop 0
  where
    loop k
        | k < B.length xs && k < B.length ys
            && xs `B.index` k == ys `B.index` k
            = loop (k+1)
        | otherwise = (B.take k xs, B.drop k xs, B.drop k ys)

normalizeErrs = BC.pack . loop . BC.unpack
  where
    loop ('\239':'\191':'\189':rest) = loop rest
    loop ('?':rest) = loop rest
    loop (c:cs) = c : loop cs
    loop [] = []


testI :: Invocation -> [B.ByteString] -> [B.ByteString] -> Test
testI i inp outp = test $ assertInvocation i inp outp
