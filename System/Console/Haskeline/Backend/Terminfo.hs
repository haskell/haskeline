module System.Console.Haskeline.Backend.Terminfo(
                            Draw(),
                            runTerminfoDraw
                            )
                             where

import System.Console.Terminfo
import Control.Monad
import Data.List(intersperse)
import System.IO (hFlush,stdout)
import qualified Control.Exception as Exception

import System.Console.Haskeline.Monads as Monads
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Command
import System.Console.Haskeline.Term
import System.Console.Haskeline.Backend.Posix
import qualified Codec.Binary.UTF8.String as UTF8

-- | Keep track of all of the output capabilities we can use.
-- 
-- We'll be frequently using the (automatic) 'Monoid' instance for 
-- @Actions -> TermOutput@.
data Actions = Actions {leftA, rightA, upA :: Int -> TermOutput,
                        clearToLineEnd :: TermOutput,
                        nl, cr :: TermOutput,
                        bellAudible,bellVisual :: TermOutput,
                        clearAll :: LinesAffected -> TermOutput,
                        wrapLine :: TermOutput}

getActions :: Capability Actions
getActions = do
    leftA' <- moveLeft
    rightA' <- moveRight
    upA' <- moveUp
    clearToLineEnd' <- clearEOL
    clearAll' <- clearScreen
    nl' <- newline
    cr' <- carriageReturn
    -- Don't require the bell capabilities
    bellAudible' <- bell `mplus` return mempty
    bellVisual' <- visualBell `mplus` return mempty
    wrapLine' <- getWrapLine nl' (leftA' 1)
    return Actions{leftA=leftA',rightA=rightA',upA=upA',
                clearToLineEnd=clearToLineEnd',nl=nl',cr=cr',
                bellAudible=bellAudible', bellVisual=bellVisual',
                clearAll=clearAll',
                 wrapLine=wrapLine'}

text :: String -> Actions -> TermOutput
text str _ = termText (UTF8.encodeString str)

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

newtype Draw m a = Draw {unDraw :: ReaderT Actions (ReaderT Terminal (StateT TermPos m)) a}
    deriving (Monad,MonadIO,MonadReader Actions,MonadReader Terminal,
        MonadState TermPos)

instance MonadReader Layout m => MonadReader Layout (Draw m) where
    ask = lift ask
    local r = Draw . local r . unDraw

instance MonadException m => MonadException (Draw m) where
    block = Draw . block . unDraw
    unblock = Draw . unblock . unDraw
    catch (Draw f) g = Draw $ Monads.catch f (unDraw . g)


instance MonadTrans Draw where
    lift = Draw . lift . lift . lift
    
runTerminfoDraw :: (MonadException m, MonadLayout m) => IO (Maybe (RunTerm m))
runTerminfoDraw = do
    mterm <- Exception.try setupTermFromEnv
    case mterm of
        Left _ -> return Nothing
        Right term -> case getCapability term getActions of
            Nothing -> return Nothing
            Just actions -> return $ Just $ RunTerm {
                getLayout = getPosixLayout,
                withGetEvent = withPosixGetEvent (Just term),
                putStrTerm = putStr . UTF8.encodeString,
                runTerm = \(Draw f) -> evalStateT' initTermPos 
                                    $ runReaderT' term
                                    $ runReaderT' actions f
                }
    
output :: MonadIO m => (Actions -> TermOutput) -> Draw m ()
output f = do
    toutput <- asks f
    term <- ask
    liftIO $ runTermOutput term toutput
    liftIO $ hFlush stdout



changeRight, changeLeft :: MonadLayout m => Int -> Draw m ()
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
printText :: MonadLayout m => String -> Draw m ()
printText "" = return ()
printText xs = fillLine xs >>= printText

-- Draws as much of the string as possible in the line, and returns the rest.
-- If we fill up the line completely, wrap to the next row.
fillLine :: MonadLayout m => String -> Draw m String
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

drawLineDiffT :: (LineState s, LineState t, MonadLayout m) 
                        => String -> s -> t -> Draw m ()
drawLineDiffT prefix s1 s2 = let 
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

linesLeft :: Layout -> TermPos -> Int -> Int
linesLeft Layout {width=w} TermPos {termCol = c} n
    | c + n < w = 1
    | otherwise = 1 + div (c+n) w

lsLinesLeft :: LineState s => Layout -> TermPos -> s -> Int
lsLinesLeft layout pos s = linesLeft layout pos (lengthToEnd s)

clearDeadText :: MonadLayout m => Int -> Draw m ()
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

clearLayoutT :: MonadLayout m => Draw m ()
clearLayoutT = do
    h <- asks height
    output (flip clearAll h)
    put initTermPos

moveToNextLineT :: (LineState s, MonadLayout m) => s -> Draw m ()
moveToNextLineT s = do
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

withRepositionT :: MonadReader Layout m => Layout -> Draw m a -> Draw m a
withRepositionT newLayout f = do
    oldPos <- get
    oldLayout <- ask
    let newPos = reposition oldLayout newLayout oldPos
    put newPos
    local newLayout f

instance MonadLayout m => Term (Draw m) where
    drawLineDiff = drawLineDiffT
    withReposition = withRepositionT
    
    printLines [] = return ()
    printLines ls = output $ mconcat $ intersperse nl (map text ls) ++ [nl] 
    clearLayout = clearLayoutT
    moveToNextLine = moveToNextLineT
    ringBell True = output bellAudible
    ringBell False = output bellVisual
