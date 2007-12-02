module LineState where

import System.Console.Terminfo
import Control.Monad

-- | Keep track of all of the output capabilities we can use.
data Actions = Actions {left, right :: Int -> TermOutput,
                        clearToLineEnd :: TermOutput,
                        nl :: TermOutput}

-- TODO
-- type Action = Action -> TermOutput

getActions :: Capability Actions
getActions = liftM4 Actions moveLeft moveRight clearEOL newline

type LineChange = LineState -> (LineState, Actions -> TermOutput)

noChange :: Actions -> TermOutput
noChange _ = mempty

refreshAfterCursor :: String -> Actions -> TermOutput
refreshAfterCursor ys actions = mconcat [clearToLineEnd actions,
                                           termText ys,
                                           left actions (length ys)]


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
moveToStart (LS xs ys) = (LS [] (reverse xs ++ ys), flip left (length xs))
moveToEnd (LS xs ys) = (LS (reverse ys ++ xs) [], flip right (length ys))


goLeft, goRight, deleteNext, deletePrev :: LineChange
goLeft ls@(LS [] _) = (ls,noChange) 
goLeft (LS (x:xs) ys) = (LS xs (x:ys), flip left 1)

goRight ls@(LS _ []) = (ls,noChange)
goRight (LS ys (x:xs)) = (LS (x:ys) xs, flip right 1)

-- add a character to the left of the cursor
insertChar :: Char -> LineChange
insertChar c (LS xs ys) =  (LS (c:xs) ys, 
                            \actions -> termText [c]
                            -- TODO: no need for clearEOL
                                    `mappend` refreshAfterCursor ys actions)

deleteNext ls@(LS _ []) = (ls, noChange)
deleteNext (LS xs (y:ys)) = (LS xs ys, refreshAfterCursor ys)

deletePrev ls@(LS [] _) = (ls,noChange)
deletePrev (LS (x:xs) ys) = (LS xs ys, 
                             \actions -> left actions 1
                                    `mappend` refreshAfterCursor ys actions)

{--
-- TODO: don't always need to refresh whole line
-- also, use variety of methods instead of just moveLeft (eg hpa,
-- cr+moveRight)
refreshLS :: Capability (LineState -> LineState -> TermOutput)
refreshLS = do
    clrEOS <- clearEOS -- don't just clear to end of line, because we might
                        -- be on multiple lines in the display.
    left <- moveLeft 
    return (\ls ls' -> mconcat [left $ fst $ lengths ls, 
                                termText (lineContents ls'),
                                clrEOS 1, left $ snd $ lengths ls'])
--}
{-
printLS :: Capability (String -> LineState -> TermOutput)
printLS = do
    refresh <- refreshLS
    return (\s ls -> mconcat [termText s, refresh ls])
-}
