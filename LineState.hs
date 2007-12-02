module LineState where

import System.Console.Terminfo
import Control.Monad

-- | Keep track of all of the output capabilities we can use.
-- 
-- We'll be frequently using the (automatic) 'Monoid' instance for 
-- @Actions -> TermOutput@.
data Actions = Actions {leftA, rightA :: Int -> TermOutput,
                        clearToLineEnd :: TermOutput,
                        nl :: TermOutput}

getActions :: Capability Actions
getActions = liftM4 Actions moveLeft moveRight clearEOL newline

text :: String -> Actions -> TermOutput
text str _ = termText str

left,right :: Int -> Actions -> TermOutput
left = flip leftA
right = flip rightA

type LineChange = LineState -> (LineState, Actions -> TermOutput)

addAfterCursor, refreshAfterCursor :: String -> Actions -> TermOutput
addAfterCursor ys = mconcat [text ys, left (length ys)]
refreshAfterCursor ys = mconcat [clearToLineEnd, text ys, left (length ys)]


data LineState = LS String -- characters to left of cursor, reversed
                    String -- characters under and to right of cursor

instance Show LineState where
    show (LS xs ys) = show [reverse xs,ys]

lineContents :: LineState -> String
lineContents (LS xs ys) = reverse xs ++ ys

lengths :: LineState -> (Int,Int)
lengths (LS xs ys) = (length xs, length ys)

-- start before first character
lineState :: String -> LineState
lineState s = LS [] s


moveToStart, moveToEnd :: LineChange
moveToStart (LS xs ys) = (LS [] (reverse xs ++ ys), left (length xs))
moveToEnd (LS xs ys) = (LS (reverse ys ++ xs) [], right (length ys))


goLeft, goRight, deleteNext, deletePrev :: LineChange
goLeft ls@(LS [] _) = (ls, mempty) 
goLeft (LS (x:xs) ys) = (LS xs (x:ys), left 1)

goRight ls@(LS _ []) = (ls, mempty)
goRight (LS ys (x:xs)) = (LS (x:ys) xs, right 1)

-- add a character to the left of the cursor
insertChar :: Char -> LineChange
insertChar c (LS xs ys) =  (LS (c:xs) ys, 
                            text [c] `mappend` addAfterCursor ys)

deleteNext ls@(LS _ []) = (ls, mempty)
deleteNext (LS xs (y:ys)) = (LS xs ys, refreshAfterCursor ys)

deletePrev ls@(LS [] _) = (ls, mempty)
deletePrev (LS (x:xs) ys) = (LS xs ys, 
                              left 1 `mappend` refreshAfterCursor ys)

