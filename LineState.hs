module LineState where

import System.Console.Terminfo

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


moveToStart, moveToEnd :: LineState -> LineState 
moveToStart (LS xs ys) = LS [] (reverse xs ++ ys)
moveToEnd (LS xs ys) = LS (reverse ys ++ xs) []


goLeft, goRight, deleteNext, deletePrev :: LineState -> LineState
goLeft ls@(LS [] _) = ls
goLeft (LS (x:xs) ys) = LS xs (x:ys)

goRight ls@(LS _ []) = ls
goRight (LS ys (x:xs)) = LS (x:ys) xs

-- add a character to the left of the cursor
insertChar :: Char -> LineState -> LineState
insertChar c (LS xs ys) = LS (c:xs) ys

deleteNext ls@(LS _ []) = ls
deleteNext (LS xs (y:ys)) = LS xs ys

deletePrev ls@(LS [] _) = ls
deletePrev (LS (x:xs) ys) = LS xs ys

data LineChange = GoLeft | GoRight | DeleteLeft | DeleteRight 
                    | InsertChar Char

lineChange :: LineChange -> LineState -> LineState
lineChange GoLeft = goLeft
lineChange GoRight = goRight
lineChange DeleteLeft = deletePrev
lineChange DeleteRight = deleteNext
lineChar (InsertChar c) = insertChar c

{-
getLineChangeCap :: Capability (LineChange -> TermOutput)
getLineChangeCap 
-}

-- TODO: don't always need to refresh whole line
-- also, use variety of methods instead of just moveLeft (eg hpa,
-- cr+moveRight)
-- BUG: need to move back based on the prev linestate, not the next one.
refreshLS :: Capability (LineState -> LineState -> TermOutput)
refreshLS = do
    clrEOS <- clearEOS -- don't just clear to end of line, because we might
                        -- be on multiple lines in the display.
    left <- moveLeft 
    return (\ls ls' -> mconcat [left $ fst $ lengths ls, 
                                termText (lineContents ls'),
                                clrEOS 1, left $ snd $ lengths ls'])

{-
printLS :: Capability (String -> LineState -> TermOutput)
printLS = do
    refresh <- refreshLS
    return (\s ls -> mconcat [termText s, refresh ls])
-}
