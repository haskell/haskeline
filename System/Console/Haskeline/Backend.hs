module System.Console.Haskeline.Backend where

import System.Console.Haskeline.Term

#ifdef MINGW
import System.Console.Haskeline.Backend.Win32 as Win32
#else
#ifdef TERMINFO
import System.Console.Haskeline.Backend.Terminfo as Terminfo
#endif
import System.Console.Haskeline.Backend.DumbTerm as DumbTerm
#endif

myRunTerm :: IO RunTerm

#ifdef MINGW
myRunTerm = win32Term
#else
#ifndef TERMINFO
myRunTerm = runDumbTerm
#else
myRunTerm = do
    mRun <- runTerminfoDraw
    case mRun of 
        Nothing -> runDumbTerm
        Just run -> return run
#endif
#endif
