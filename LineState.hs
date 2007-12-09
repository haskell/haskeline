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


diffLineStates :: LineState -> LineState -> Actions -> TermOutput
diffLineStates (LS xs1 ys1) (LS xs2 ys2) = 
    case matchInit (reverse xs1) (reverse xs2) of
        ([],[])     | ys1 == ys2            -> mempty
        (xs1',[])   | xs1' ++ ys1 == ys2    -> left (length xs1')
        ([],xs2')   | ys1 == xs2' ++ ys2    -> right (length xs2')
        (xs1',xs2')                         -> mconcat [left (length xs1'),
                                                text xs2', textForward ys2]
-- TODO: if new length left of cursor is greater than before, no need to
-- clear to line end.
textForward s = mconcat [clearToLineEnd,text s,left (length s)]

matchInit :: Eq a => [a] -> [a] -> ([a],[a])
matchInit (x:xs) (y:ys)  | x == y = matchInit xs ys
matchInit xs ys = (xs,ys)



type LineChange = LineState -> LineState
moveToStart, moveToEnd :: LineChange
moveToStart (LS xs ys) = LS [] (reverse xs ++ ys)
moveToEnd (LS xs ys) = LS (reverse ys ++ xs) []


goLeft, goRight, deleteNext, deletePrev :: LineChange
goLeft ls@(LS [] _) = ls 
goLeft (LS (x:xs) ys) = LS xs (x:ys)

goRight ls@(LS _ []) = ls
goRight (LS ys (x:xs)) = LS (x:ys) xs

-- add a character to the left of the cursor
insertChar :: Char -> LineChange
insertChar c (LS xs ys) =  LS (c:xs) ys

deleteNext ls@(LS _ []) = ls
deleteNext (LS xs (y:ys)) = LS xs ys

deletePrev ls@(LS [] _) = ls
deletePrev (LS (x:xs) ys) = LS xs ys 

