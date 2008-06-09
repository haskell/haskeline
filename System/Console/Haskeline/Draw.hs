module System.Console.Haskeline.Draw(Actions(),
                            getActions,
                            Draw(),
                            runDraw,
                            drawLine,
                            withReposition,
                            moveToNextLine,
                            drawEffect)
                             where

import System.Console.Terminfo
import Control.Monad
import Data.List(intersperse)

import System.Console.Haskeline.Monads
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Command
import System.Console.Haskeline.InputT

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
getWrapLine nl' left1 = (autoRightMargin >>= guard >> withAutoMargin)
                    `mplus` return nl'
  where 
    -- If the wraparound glitch is in effect, force a wrap by printing a space.
    -- Otherwise, it'll wrap automatically.
    withAutoMargin = (do
                        wraparoundGlitch >>= guard
                        return (termText " " <#> left1)
                     )`mplus` return mempty
    
left,right,up :: Int -> Actions -> TermOutput
left = flip leftA
right = flip rightA
up = flip upA


--------


mreplicate :: Monoid m => Int -> m -> m
mreplicate n m
    | n <= 0    = mempty
    | otherwise = m `mappend` mreplicate (n-1) m

-- denote in modular arithmetic;
-- in particular, 0 <= termCol < width
data TermPos = TermPos {termRow,termCol :: Int}
    deriving Show

initTermPos :: TermPos
initTermPos = TermPos {termRow = 0, termCol = 0}


--------------

newtype Draw m a = Draw (ReaderT Actions (ReaderT Terminal (StateT TermPos m)) a)
    deriving (Monad,MonadIO,MonadReader Actions,MonadReader Terminal,
        MonadState TermPos)

instance MonadTrans Draw where
    lift = Draw . lift . lift . lift
    lift2 f (Draw m) = Draw $ lift2 (lift2 (lift2 f)) m
    
-- TODO: cache the terminal, actions?
runDraw :: MonadIO m => Draw m a -> m a
runDraw (Draw f) = do
    term <- liftIO setupTermFromEnv
    -- TODO: use one-line dumb terminal if the below fails.
    let Just actions = getCapability term getActions
    evalStateT initTermPos $ evalReaderT term
        $ evalReaderT actions f



output :: MonadIO m => (Actions -> TermOutput) -> Draw m ()
output f = do
    toutput <- asks f
    term <- ask
    liftIO $ runTermOutput term toutput



changeRight, changeLeft :: (MonadIO m) => Int -> Draw (InputCmdT m) ()
changeRight n = do
    w <- asks width
    TermPos {termRow=r,termCol=c} <- get
    if c+n < w  
        then do
                put TermPos {termRow=r,termCol=c+n}
                output (right n)
        else do
              let m = c+n
              let linesDown = m `div` w
              let newCol = m `rem` w
              put TermPos {termRow=r+linesDown, termCol=newCol}
              output $ cr <#> mreplicate linesDown nl <#> right newCol
                      
changeLeft n = do
    w <- asks width
    TermPos {termRow=r,termCol=c} <- get
    if c - n >= 0 
        then do 
                put TermPos {termRow = r,termCol = c-n}
                output (left n)
        else do      
                let m = n - c
                let linesUp = 1 + ((m-1) `div` w)
                let newCol = (-m) `mod` w -- mod returns positive #
                put TermPos {termRow = r - linesUp, termCol=newCol}
                output $ cr <#> up linesUp <#> right newCol
                
-- TODO: I think if we wrap this all up in one call to output, it'll be faster...
printText :: (MonadIO m) => String -> Draw (InputCmdT m) ()
printText "" = return ()
printText xs = fillLine xs >>= printText

-- Draws as much of the string as possible in the line, and returns the rest.
-- If we fill up the line completely, wrap to the next row.
fillLine :: (MonadIO m) => String -> Draw (InputCmdT m) String
fillLine str = do
    w <- asks width
    TermPos {termRow=r,termCol=c} <- get
    let roomLeft = w - c
    if length str < roomLeft
        then do
                output (text str)
                put TermPos{termRow=r, termCol=c+length str}
                return ""
        else do
                let (thisLine,rest) = splitAt roomLeft str
                output (text thisLine <#> wrapLine)
                put TermPos {termRow=r+1,termCol=0}
                return rest

diffLinesBreaking :: (LineState s, LineState t, MonadIO m) 
                        => String -> s -> t -> Draw (InputCmdT m) ()
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

clearDeadText :: (MonadIO m) => Int -> Draw (InputCmdT m) ()
clearDeadText n
    | n <= 0    = return ()
    | otherwise = do
        layout <- ask
        pos <- get
        let numLinesToClear = linesLeft layout pos n
        output clearToLineEnd
        when (numLinesToClear > 1) $ output $ mconcat [
                    mreplicate (numLinesToClear - 1) 
                            $ nl <#> clearToLineEnd
                    , up (numLinesToClear - 1)
                    , right (termCol pos)]

drawLine :: (LineState s, MonadIO m) => String -> s -> Draw (InputCmdT m) ()
drawLine prefix s = do
    printText (beforeCursor prefix s ++ afterCursor s)
    changeLeft (lengthToEnd s)

redrawLine :: (LineState s, MonadIO m) => String -> s -> Draw (InputCmdT m) ()
redrawLine prefix s = do
    pos <- get
    output $ left (termCol pos) <#> up (termRow pos)
    put initTermPos
    drawLine prefix s

clearScreenAndRedraw :: (LineState s, MonadIO m) => String -> s -> Draw (InputCmdT m) ()
clearScreenAndRedraw prefix s = do
    h <- asks height
    output (flip clearAll h)
    put initTermPos
    drawLine prefix s

moveToNextLine :: (LineState s, MonadIO m) => s -> Draw (InputCmdT m) ()
moveToNextLine s = do
    pos <- get
    layout <- ask
    output $ mreplicate (lsLinesLeft layout pos s) nl
    put initTermPos


posFromLength :: Layout -> Int -> TermPos
posFromLength Layout {width = w} n = TermPos 
                            {termRow = n `div` w, termCol = n `mod` w}

posToLength :: Layout -> TermPos -> Int
posToLength Layout {width = w} TermPos {termRow = r, termCol = c}
    = r * w + c

reposition :: Layout -> Layout -> TermPos -> TermPos
reposition oldLayout newLayout oldPos = posFromLength newLayout $ 
                                            posToLength oldLayout oldPos

withReposition :: Monad m => Layout -> Draw (InputCmdT m) a -> Draw (InputCmdT m) a
withReposition newLayout f = do
    oldPos <- get
    oldLayout <- ask
    let newPos = reposition oldLayout newLayout oldPos
    put newPos
    lift2 (local newLayout) f


drawEffect :: (LineState s,LineState t, MonadIO m)
        => String -> s -> Effect t -> Draw (InputCmdT m) ()
drawEffect prefix s (Redraw shouldClear t) = do
            if shouldClear
                then clearScreenAndRedraw prefix s
                else redrawLine prefix t
drawEffect prefix s (Change t) = do
            diffLinesBreaking prefix s t
drawEffect prefix s (PrintLines ls t overwrite) = do
            if overwrite
                then diffLinesBreaking prefix s Cleared
                else moveToNextLine s
            output $ mconcat $ intersperse nl $ map text ls
            when (not (null ls)) $ output nl
            drawLine prefix t

