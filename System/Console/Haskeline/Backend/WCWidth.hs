module System.Console.Haskeline.Backend.WCWidth(
                            gsWidth,
                            splitAtWidth,
                            ) where

-- Certain characters are "wide", i.e. take up two spaces in the terminal.
-- This module wraps the necessary foreign routines, and also provides some convenience
-- functions for width-breaking code.

import System.Console.Haskeline.LineState

import Data.Char
import Data.List
import Foreign.C.Types

foreign import ccall unsafe mk_wcwidth :: CWchar -> Int

wcwidth :: Char -> Int
wcwidth c = case mk_wcwidth $ toEnum $ fromEnum c of
                -1 -> 0 -- shouldn't happen, since control characters
                        -- aren't turned into graphemes; but better to be safe.
                w -> w

gWidth :: Grapheme -> Int
gWidth g = wcwidth (baseChar g)

gsWidth :: [Grapheme] -> Int
gsWidth = foldl' (+) 0 . map gWidth

-- | Split off the maximal list which is no more than the given width.
-- returns the amount of extra width not used.
splitAtWidth :: Int -> [Grapheme] -> ([Grapheme],[Grapheme],Int)
splitAtWidth w [] = ([],[],w)
splitAtWidth w (g:gs)
    | gw > w = ([],g:gs,w)
    | otherwise = (g:gs',gs'',r)
  where
    gw = gWidth g
    (gs',gs'',r) = splitAtWidth (w-gw) gs
