module System.Console.Haskeline.Backend where

import System.Console.Haskeline.Term
import System.Console.Haskeline.Monads
import Control.Monad
import System.IO (stdin, hGetEcho, Handle)

#ifdef MINGW
import System.Console.Haskeline.Backend.Win32 as Win32
#else
import System.Console.Haskeline.Backend.Posix as Posix
#ifdef TERMINFO
import System.Console.Haskeline.Backend.Terminfo as Terminfo
#endif
import System.Console.Haskeline.Backend.DumbTerm as DumbTerm
#endif


defaultRunTerm :: IO RunTerm
defaultRunTerm = (liftIO (hGetEcho stdin) >>= guard >> stdinTTY)
                    `orElse` fileHandleRunTerm stdin

stdinTTY :: MaybeT IO RunTerm
#ifdef MINGW
stdinTTY = win32Term
#else
#ifndef TERMINFO
stdinTTY = runDumbTerm
#else
stdinTTY = runTerminfoDraw `mplus` runDumbTerm
#endif
#endif

fileHandleRunTerm :: Handle -> IO RunTerm
#ifdef MINGW
fileHandleRunTerm = Win32.fileRunTerm
#else
fileHandleRunTerm = Posix.fileRunTerm
#endif
