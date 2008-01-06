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
                        nl, cr :: TermOutput,
                        clearAll :: LinesAffected -> TermOutput,
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
                clearAll=clearAll',
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
                        return (termText " " <#> left1)
                     )`mplus` return mempty
    
left,right,up,down :: Int -> Actions -> TermOutput
left = flip leftA
right = flip rightA
up = flip upA
down = flip downA


class LineState s where
    beforeCursor :: String -> s -> String -- text to left of cursor
    afterCursor :: s -> String -- text under and to right of cursor
    toResult :: s -> String

lengthToEnd :: LineState s => s -> Int
lengthToEnd = length . afterCursor

class LineState s => FromString s where
    fromString :: String -> s


--------

data Layout = Layout {width, height :: Int}
                    deriving Show

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
              output $ mreplicate linesDown nl <#> right newCol
                      
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
                output $ cr <#> up linesUp <#> right newCol
                
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
                output (text thisLine <#> wrapLine)
                setPos TermPos {termRow=r+1,termCol=0}
                return rest

diffLinesBreaking :: (LineState s, LineState t, MonadIO m) 
                        => String -> s -> t -> Draw m ()
diffLinesBreaking prefix s1 s2 = let 
    xs1 = beforeCursor prefix s1
    ys1 = afterCursor s1
    xs2 = beforeCursor prefix s2
    ys2 = afterCursor s2
    in case matchInit xs1 xs2 of
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

lsLinesLeft :: LineState s => Layout -> TermPos -> s -> Int
lsLinesLeft layout pos s = linesLeft layout pos (lengthToEnd s)

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
                            $ nl <#> clearToLineEnd
                    , up (numLinesToClear - 1)
                    , right (termCol pos)]

drawLine :: (LineState s, MonadIO m) => String -> s -> Draw m ()
drawLine prefix s = do
    printText (beforeCursor prefix s ++ afterCursor s)
    changeLeft (lengthToEnd s)

redrawLine :: (LineState s, MonadIO m) => String -> s -> Draw m ()
redrawLine prefix s = do
    pos <- getPos
    output $ left (termCol pos) <#> up (termRow pos)
    setPos initTermPos
    drawLine prefix s

clearScreenAndRedraw :: (LineState s, MonadIO m) => String -> s -> Draw m ()
clearScreenAndRedraw prefix s = do
    h <- liftM height askLayout
    output (flip clearAll h)
    setPos initTermPos
    drawLine prefix s

moveToNextLine :: (LineState s, MonadIO m) => s -> Draw m ()
moveToNextLine s = do
    pos <- getPos
    layout <- askLayout
    output $ mreplicate (lsLinesLeft layout pos s) nl
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
