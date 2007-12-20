module System.Console.HaskLine.LineState where

import System.Console.Terminfo
import Control.Monad
import Control.Monad.Trans

-- | Keep track of all of the output capabilities we can use.
-- 
-- We'll be frequently using the (automatic) 'Monoid' instance for 
-- @Actions -> TermOutput@.
data Actions = Actions {leftA, rightA, upA, downA :: Int -> TermOutput,
                        clearToLineEnd :: TermOutput,
                        nl, cr, clearAll :: TermOutput,
                        wrapLine :: TermOutput}

getActions :: Capability Actions
getActions = do
    leftA' <- moveLeft
    rightA' <- moveRight
    upA' <- moveUp
    downA' <- moveDown
    clearToLineEnd' <- clearEOL
    clearAll' <- clearScreen
    nl' <- newline
    cr' <- carriageReturn
    wrapLine' <- getWrapLine nl' (leftA' 1)
    return Actions{leftA=leftA',rightA=rightA',upA=upA',downA=downA',
                clearToLineEnd=clearToLineEnd',nl=nl',cr=cr',
                clearAll=clearAll' 1,
                 wrapLine=wrapLine'}

text :: String -> Actions -> TermOutput
text str _ = termText str

getWrapLine :: TermOutput -> TermOutput -> Capability TermOutput
getWrapLine nl left1 = (autoRightMargin >>= guard >> withAutoMargin)
                    `mplus` return nl
  where 
    -- If the wraparound glitch is in effect, force a wrap by printing a space.
    -- Otherwise, it'll wrap automatically.
    withAutoMargin = (do
                        wraparoundGlitch >>= guard
                        return (termText " " `mappend` left1)
                     )`mplus` return mempty
    
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


type LineChange = LineState -> LineState

moveToStart, moveToEnd, killLine :: LineChange
moveToStart (LS xs ys) = LS [] (reverse xs ++ ys)
moveToEnd (LS xs ys) = LS (reverse ys ++ xs) []
killLine (LS xs ys) = LS [] ys


goLeft, goRight, deleteNext, deletePrev :: LineChange
goLeft ls@(LS [] _) = ls 
goLeft (LS (x:xs) ys) = LS xs (x:ys)

goRight ls@(LS _ []) = ls
goRight (LS ys (x:xs)) = LS (x:ys) xs

-- add a character to the left of the cursor
insertChar :: Char -> LineChange
insertChar c (LS xs ys) =  LS (c:xs) ys

insertText :: String -> LineChange
insertText s (LS xs ys) = LS (reverse s ++ xs) ys

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

initTermPos = TermPos {termRow = 0, termCol = 0}


--------------

newtype Draw m a = Draw (Actions -> Terminal -> Layout
                          -> TermPos -> m (TermPos, a))

runDraw :: Monad m => Actions -> Terminal -> Layout -> Draw m a -> m a
runDraw actions term layout (Draw f) = do
    (p,x) <- f actions term layout initTermPos
    return x

instance Monad m => Monad (Draw m) where
    return x = Draw $ \_ _ _ pos -> return (pos,x)
    Draw f >>= g = Draw $ \actions term layout pos1 -> do
                                (pos2,y) <- f actions term layout pos1 
                                let Draw g' = g y
                                g' actions term layout pos2

instance MonadTrans Draw where
    lift f = Draw $ \_ _ _ pos -> do {x <- f; return (pos,x)}

instance MonadIO m => MonadIO (Draw m) where
    liftIO f = Draw $ \_ _ _ pos -> do {x <- liftIO f; return (pos,x)}

getPos :: Monad m => Draw m TermPos
getPos = Draw (\_ _ _ pos -> return (pos,pos))

setPos :: Monad m => TermPos -> Draw m ()
setPos pos = Draw (\_ _ _ _ -> return (pos,()))

askLayout :: Monad m => Draw m Layout
askLayout = Draw (\_ _ layout pos -> return (pos,layout))

withLayout :: Monad m => Layout -> Draw m a -> Draw m a
withLayout layout (Draw f) = Draw $ \actions term _ pos 
                                    -> f actions term layout pos

output :: MonadIO m => (Actions -> TermOutput) -> Draw m ()
output f = Draw $ \actions term _ pos -> do
    liftIO $ runTermOutput term (f actions)
    return (pos,())



changeRight :: MonadIO m => Int -> Draw m ()
changeRight n = do
    w <- liftM width askLayout
    TermPos {termRow=r,termCol=c} <- getPos
    if c+n < w  
        then do
                setPos TermPos {termRow=r,termCol=c+n}
                output (right n)
        else do
              let m = c+n
              let linesDown = m `div` w
              let newCol = m `rem` w
              setPos TermPos {termRow=r+linesDown, termCol=newCol}
              output $ mconcat [mreplicate linesDown nl, right newCol]
                      
changeLeft :: MonadIO m => Int -> Draw m ()
changeLeft n = do
    w <- liftM width askLayout
    TermPos {termRow=r,termCol=c} <- getPos
    if c - n >= 0 
        then do 
                setPos TermPos {termRow = r,termCol = c-n}
                output (left n)
        else do      
                let m = n - c
                let linesUp = 1 + (m `div` w)
                let linesFromEnd = m `rem` w
                let newCol = w - linesFromEnd
                setPos TermPos {termRow = r - linesUp, termCol=newCol}
                output $ mconcat [cr, up linesUp, right newCol]
                
-- TODO: I think if we wrap this all up in one call to output, it'll be faster...
printText :: MonadIO m => String -> Draw m ()
printText "" = return ()
printText xs = fillLine xs >>= printText

-- Draws as much of the string as possible in the line, and returns the rest.
-- If we fill up the line completely, wrap to the next row.
fillLine :: MonadIO m => String -> Draw m String
fillLine str = do
    w <- liftM width askLayout
    TermPos {termRow=r,termCol=c} <- getPos
    let roomLeft = w - c
    if length str < roomLeft
        then do
                output (text str)
                setPos TermPos{termRow=r, termCol=c+length str}
                return ""
        else do
                let (thisLine,rest) = splitAt roomLeft str
                output (text thisLine `mappend` wrapLine)
                setPos TermPos {termRow=r+1,termCol=0}
                return rest

diffLinesBreaking :: MonadIO m => LineState -> LineState -> Draw m ()
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

matchInit :: Eq a => [a] -> [a] -> ([a],[a])
matchInit (x:xs) (y:ys)  | x == y = matchInit xs ys
matchInit xs ys = (xs,ys)


linesLeft :: Layout -> TermPos -> Int -> Int
linesLeft Layout {width=w} TermPos {termCol = c} n
    | c + n < w = 1
    | otherwise = 1 + div (c+n) w

lsLinesLeft :: Layout -> TermPos -> LineState -> Int
lsLinesLeft layout pos (LS _ ys) = linesLeft layout pos (length ys)

clearDeadText :: MonadIO m => Int -> Draw m ()
clearDeadText n
    | n <= 0    = return ()
    | otherwise = do
        layout <- askLayout
        pos <- getPos
        let numLinesToClear = linesLeft layout pos n
        output clearToLineEnd
        when (numLinesToClear > 1) $ output $ mconcat [
                    mreplicate (numLinesToClear - 1) 
                            $ nl `mappend` clearToLineEnd
                    , up (numLinesToClear - 1)
                    , right (termCol pos)]

drawLine :: MonadIO m => String -> LineState -> Draw m ()
drawLine prefix (LS xs ys) = do
    printText (prefix ++ reverse xs ++ ys)
    changeLeft (length ys)

redrawLine :: MonadIO m => String -> LineState -> Draw m ()
redrawLine prefix ls = do
    pos <- getPos
    output (left (termCol pos) `mappend` up (termRow pos))
    setPos initTermPos
    drawLine prefix ls

clearScreenAndRedraw :: MonadIO m => String -> LineState -> Draw m ()
clearScreenAndRedraw prefix ls = do
    output clearAll
    setPos initTermPos
    drawLine prefix ls

moveToNextLine :: MonadIO m => LineState -> Draw m ()
moveToNextLine ls = do
    pos <- getPos
    layout <- askLayout
    output $ mreplicate (lsLinesLeft layout pos ls) nl
    setPos initTermPos


posFromLength :: Layout -> Int -> TermPos
posFromLength Layout {width = w} n = TermPos 
                            {termRow = n `div` w, termCol = n `mod` w}

posToLength :: Layout -> TermPos -> Int
posToLength Layout {width = w} TermPos {termRow = r, termCol = c}
    = r * w + c

reposition :: Layout -> Layout -> TermPos -> TermPos
reposition oldLayout newLayout oldPos = posFromLength newLayout $ 
                                            posToLength oldLayout oldPos

withReposition :: Monad m => Layout -> Draw m a -> Draw m a
withReposition newLayout f = do
    oldPos <- getPos
    oldLayout <- askLayout
    let newPos = reposition oldLayout newLayout oldPos
    setPos newPos
    withLayout newLayout f
