{-# LANGUAGE RecordWildCards #-}
-- This module provides an interface for testing the output
-- of programs that expect to be run in a terminal.
module RunTTY (Invocation(..),
            runInvocation,
            assertInvocation,
            testI,
            setLang,
            setTerm,
            setLatin1,
            setUTF8
            ) where

import Control.Concurrent
import Data.ByteString as B
import System.IO
import System.Process
import Test.HUnit
import qualified Data.ByteString.Char8 as BC

import Pty


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

setLang :: String -> Invocation -> Invocation
setLang = setEnv "LANG"
setTerm :: String -> Invocation -> Invocation
setTerm = setEnv "TERM"


setUTF8 :: Invocation -> Invocation
setUTF8 = setLang "C.UTF-8"
setLatin1 :: Invocation -> Invocation
setLatin1 = setLang "C.ISO8859-1"


runInvocation :: Invocation
        -> [B.ByteString] -- Input chunks.  (We pause after each chunk to
                        -- simulate real user input and prevent Haskeline
                        -- from coalescing the changes.)
        -> IO [B.ByteString]
runInvocation Invocation {..} inputs
    | runInTTY = runCommandInPty prog progArgs (Just environment) inputs
    | otherwise = do
    (Just inH, Just outH, Nothing, ph)
        <- createProcess (proc prog progArgs)
                            { env = Just environment
                            , std_in = CreatePipe
                            , std_out = CreatePipe
                            , std_err = Inherit
                            }
    hSetBuffering inH NoBuffering
    firstOutput <- getOutput outH
    outputs <- mapM (inputOutput inH outH) inputs
    hClose inH
    lastOutput <- getOutput outH -- output triggered by EOF, if any
    terminateProcess ph
    _ <- waitForProcess ph
    return $ firstOutput : outputs
                ++ if B.null lastOutput then [] else [lastOutput]

inputOutput :: Handle -> Handle -> B.ByteString -> IO B.ByteString
inputOutput inH outH input = do
    B.hPut inH input
    getOutput outH


getOutput :: Handle -> IO B.ByteString
getOutput h = do
    threadDelay 20000
    B.hGetNonBlocking h 4096


assertInvocation :: Invocation -> [B.ByteString] -> [B.ByteString]
                    -> Assertion
assertInvocation i input expectedOutput = do
    actualOutput <- runInvocation i input
    assertSameList expectedOutput $ fmap fixOutput actualOutput

-- Remove CRLFs from output, since tty translates all LFs into CRLF.
-- (TODO: I'd like to just unset ONLCR in the slave tty, but
-- System.Posix.Terminal doesn't support that flag.)
fixOutput :: B.ByteString -> B.ByteString
fixOutput = BC.pack . loop . BC.unpack
  where
    loop ('\r':'\n':rest) = '\n' : loop rest
    loop (c:cs) = c : loop cs
    loop [] = []

assertSameList :: (Show a, Eq a) => [a] -> [a] -> Assertion
assertSameList [] [] = return ()
assertSameList (x:xs) (y:ys)
    | x == y = assertSameList xs ys
assertSameList xs ys = xs @=? ys -- cause error to be thrown

testI :: Invocation -> [B.ByteString] -> [B.ByteString] -> Test
testI i inp outp = test $ assertInvocation i inp outp
