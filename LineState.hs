module LineState where

import System.Console.Terminfo
import Control.Monad
import Control.Monad.RWS

-- | Keep track of all of the output capabilities we can use.
-- 
-- We'll be frequently using the (automatic) 'Monoid' instance for 
-- @Actions -> TermOutput@.
data Actions = Actions {leftA, rightA, upA, downA :: Int -> TermOutput,
                        clearToLineEnd :: TermOutput,
                        nl :: TermOutput,
                        cr :: TermOutput}

getActions :: Capability Actions
getActions = do
    leftA' <- moveLeft
    rightA' <- moveRight
    upA' <- moveUp
    downA' <- moveDown
    clearToLineEnd' <- clearEOL
    nl' <- newline
    cr' <- carriageReturn
    return Actions{leftA=leftA',rightA=rightA',upA=upA',downA=downA',
                clearToLineEnd=clearToLineEnd',nl=nl',cr=cr'}

text :: String -> Actions -> TermOutput
text str _ = termText str

left,right,up,down :: Int -> Actions -> TermOutput
left = flip leftA
right = flip rightA
up = flip upA
down = flip downA






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

--------

data Layout = Layout {width :: Int}

mreplicate :: Monoid m => Int -> m -> m
mreplicate n m
    | n <= 0    = mempty
    | otherwise = m `mappend` mreplicate (n-1) m

-- denote in modular arithmetic;
-- in particular, 0 <= termCol < width
data TermPos = TermPos {termRow,termCol :: Int}
    deriving Show

type Draw = RWS Layout (Actions -> TermOutput) TermPos

changeRight :: Int -> Draw ()
changeRight n = RWS $ \Layout {width = w} TermPos {termRow=r,termCol=c} ->
    if c+n < w  
        then ((),TermPos {termRow=r,termCol=c+n}, right n)
        else      let m = c+n
                      linesDown = m `div` w
                      newCol = m `rem` w
                  in ((),TermPos {termRow=r+linesDown, termCol=newCol},
                        mconcat [mreplicate linesDown nl, right newCol])
                      
changeLeft :: Int -> Draw ()
changeLeft n = RWS $ \Layout {width = w} TermPos {termRow=r,termCol=c} ->
    if c - n >= 0 
        then ((),TermPos {termRow = r,termCol = c-n}, left n)
        else      let m = n - c
                      linesUp = 1 + (m `div` w)
                      linesFromEnd = m `rem` w
                      newCol = w - linesFromEnd
                  in ((),TermPos {termRow = r - linesUp, termCol=newCol},
                      mconcat [cr, up linesUp, right newCol])
                
-- todo: when dealing with a whole string, computations can be more efficient.
printText :: String -> Draw ()
printText = mapM_ printChar
    where
        printChar x = do
            w <- asks width
            p@TermPos {termRow=r,termCol=c} <- get
            if c+1<w
                then do
                        put p{termCol=c+1}
                        tell (text [x])
                else do
                        put TermPos {termRow=r+1,termCol=0}
                        tell $ mconcat [text [x],nl]

diffLinesBreaking :: LineState -> LineState -> Draw ()
diffLinesBreaking (LS xs1 ys1) (LS xs2 ys2) = 
    case matchInit (reverse xs1) (reverse xs2) of
        ([],[])     | ys1 == ys2            -> return ()
        (xs1',[])   | xs1' ++ ys1 == ys2    -> changeLeft (length xs1')
        ([],xs2')   | ys1 == xs2' ++ ys2    -> changeRight (length xs2')
        (xs1',xs2')                         -> do
            changeLeft (length xs1')
            printText (xs2' ++ ys2)
            let m = length xs1' + length ys1 - (length xs2' + length ys2)
            clearDeadText m
            changeLeft (length ys2)

linesLeft :: Layout -> TermPos -> Int -> Int
linesLeft Layout {width=w} TermPos {termCol = c} n
    | c + n < w = 1
    | otherwise = 1 + div (c+n) w

lsLinesLeft :: Layout -> TermPos -> LineState -> Int
lsLinesLeft layout pos (LS _ ys) = linesLeft layout pos (length ys)

clearDeadText :: Int -> Draw ()
clearDeadText n
    | n <= 0    = return ()
    | otherwise = do
        layout <- ask
        pos <- get
        let numLinesToClear = linesLeft layout pos n
        tell clearToLineEnd
        when (numLinesToClear > 1) $ do
            replicateM (numLinesToClear - 1) $ (tell clearToLineEnd >> tell nl)
            tell $ up (numLinesToClear - 1)
            tell $ right (termCol pos)
            return ()
