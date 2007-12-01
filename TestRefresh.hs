module Main where

import System.Console.Terminfo
import Control.Monad

main = do
    t <- setupTermFromEnv
    let Just (cr,clrEOL,left,nl) = getCapability t getCaps
    let string = replicate 50 'a' ++ replicate 50 'b'
    runTermOutput t $ mconcat [cr,clrEOL, termText string, left 50]
    runTermOutput t $ mconcat [cr,clrEOL, termText string, left 50]
    runTermOutput t $ mconcat [cr,clrEOL, termText string, left 50]
    runTermOutput t nl
       
getCaps = liftM4 (,,,) carriageReturn clearEOL moveLeft newline
