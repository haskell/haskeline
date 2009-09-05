module System.Console.Haskeline.Backend.Terminfo(
                            Draw(),
                            runTerminfoDraw
                            )
                             where

import System.Console.Terminfo
import Control.Monad
import Data.List(intersperse)
import System.IO
import qualified Control.Exception.Extensible as Exception
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe, catMaybes)
import Control.Concurrent.Chan

import System.Console.Haskeline.Monads as Monads
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Term
import System.Console.Haskeline.Backend.Posix
import System.Console.Haskeline.Key

-- | Keep track of all of the output capabilities we can use.
-- 
-- We'll be frequently using the (automatic) 'Monoid' instance for 
-- @Actions -> TermOutput@.
data Actions = Actions {leftA, rightA, upA :: Int -> TermOutput,
                        clearToLineEnd :: TermOutput,
                        nl, cr :: TermOutput,
                        bellAudible,bellVisual :: TermOutput,
                        clearAllA :: LinesAffected -> TermOutput,
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
    return Actions{leftA = leftA', rightA = rightA',upA = upA',
                clearToLineEnd = clearToLineEnd', nl = nl',cr = cr',
                bellAudible = bellAudible', bellVisual = bellVisual',
                clearAllA = clearAll',
                 wrapLine = wrapLine'}

text :: B.ByteString -> Actions -> TermOutput
text str _ = termText $ B.unpack str

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

type TermAction = Actions -> TermOutput
    
left,right,up :: Int -> TermAction
left n = flip leftA n
right n = flip rightA n
up n = flip upA n

clearAll :: LinesAffected -> TermAction
clearAll la = flip clearAllA la

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

newtype Draw m a = Draw {unDraw :: (ReaderT Actions
                                    (ReaderT Terminal (StateT TermPos
                                    (PosixT m)))) a}
    deriving (Monad, MonadIO, MonadException,
              MonadReader Actions, MonadReader Terminal, MonadState TermPos,
              MonadReader Handle, MonadReader Encoders)

type DrawM a = forall m . (MonadReader Layout m, MonadIO m) => Draw m a

instance MonadTrans Draw where
    lift = Draw . lift . lift . lift . lift . lift
    
runTerminfoDraw :: IO (Maybe RunTerm)
runTerminfoDraw = do
    mterm <- Exception.try setupTermFromEnv
    ch <- newChan
    case mterm of
        -- XXX narrow this: either an ioexception (from getenv) or a 
        -- usererror.
        Left (_::SetupTermError) -> return Nothing
        Right term -> case getCapability term getActions of
            Nothing -> return Nothing
            Just actions -> fmap Just $ posixRunTerm $ \enc h ->
                TermOps {
                    getLayout = tryGetLayouts (posixLayouts h
                                                ++ [tinfoLayout term])
                    , withGetEvent = wrapKeypad h term
                                        . withPosixGetEvent ch h enc
                                            (terminfoKeys term)
                    , runTerm = \(RunTermType f) -> 
                             runPosixT enc h
                              $ evalStateT' initTermPos
                              $ runReaderT' term
                              $ runReaderT' actions
                              $ unDraw f
                    }

-- If the keypad on/off capabilities are defined, wrap the computation with them.
wrapKeypad :: MonadException m => Handle -> Terminal -> m a -> m a
wrapKeypad h term f = (maybeOutput keypadOn >> f)
                            `finally` maybeOutput keypadOff
  where
    maybeOutput cap = liftIO $ hRunTermOutput h term $
                            fromMaybe mempty (getCapability term cap)

tinfoLayout :: Terminal -> IO (Maybe Layout)
tinfoLayout term = return $ getCapability term $ do
                        r <- termColumns
                        c <- termLines
                        return Layout {height=r,width=c}

terminfoKeys :: Terminal -> [(String,Key)]
terminfoKeys term = catMaybes $ map getSequence keyCapabilities
    where
        getSequence (cap,x) = do
                            keys <- getCapability term cap
                            return (keys,x)
        keyCapabilities =
                [(keyLeft,      simpleKey LeftKey)
                ,(keyRight,      simpleKey RightKey)
                ,(keyUp,         simpleKey UpKey)
                ,(keyDown,       simpleKey DownKey)
                ,(keyBackspace,  simpleKey Backspace)
                ,(keyDeleteChar, simpleKey Delete)
                ,(keyHome,       simpleKey Home)
                ,(keyEnd,        simpleKey End)
                ,(keyPageDown,   simpleKey PageDown)
                ,(keyPageUp,     simpleKey PageUp)
                ]

    
output :: MonadIO m => TermAction -> Draw m ()
output f = do
    toutput <- asks f
    term <- ask
    ttyh <- ask
    liftIO $ hRunTermOutput ttyh term toutput



changeRight, changeLeft :: Int -> DrawM ()
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
printText :: [Grapheme] -> DrawM ()
printText [] = return ()
printText xs = fillLine xs >>= printText

-- Draws as much of the string as possible in the line, and returns the rest.
-- If we fill up the line completely, wrap to the next row.
fillLine :: [Grapheme] -> DrawM [Grapheme]
fillLine str = do
    w <- asks width
    TermPos {termRow=r,termCol=c} <- get
    let roomLeft = w - c
    if length str < roomLeft
        then do
                posixEncode (graphemesToString str) >>= output . text
                put TermPos{termRow=r, termCol=c+length str}
                return []
        else do
                let (thisLine,rest) = splitAt roomLeft str
                bstr <- posixEncode (graphemesToString thisLine)
                output (text bstr <#> wrapLine)
                put TermPos {termRow=r+1,termCol=0}
                return rest

drawLineDiffT :: LineChars -> LineChars -> DrawM ()
drawLineDiffT (xs1,ys1) (xs2,ys2) = case matchInit xs1 xs2 of
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

lsLinesLeft :: Layout -> TermPos -> LineChars -> Int
lsLinesLeft layout pos s = linesLeft layout pos (lengthToEnd s)

clearDeadText :: Int -> DrawM ()
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

clearLayoutT :: DrawM ()
clearLayoutT = do
    h <- asks height
    output (clearAll h)
    put initTermPos

moveToNextLineT :: LineChars -> DrawM ()
moveToNextLineT s = do
    pos <- get
    layout <- ask
    output $ mreplicate (lsLinesLeft layout pos s) nl
    put initTermPos

repositionT :: Layout -> LineChars -> DrawM ()
repositionT oldLayout s = do
    oldPos <- get
    let l = lsLinesLeft oldLayout oldPos s - 1
    output $ cr <#> mreplicate l nl
            <#> mreplicate (l + termRow oldPos) (clearToLineEnd <#> up 1)
    put initTermPos
    drawLineDiffT ([],[]) s

instance (MonadException m, MonadReader Layout m) => Term (Draw m) where
    drawLineDiff = drawLineDiffT
    reposition = repositionT
    
    printLines [] = return ()
    printLines ls = do
        bls <- mapM posixEncode ls
        output $ mconcat $ intersperse nl (map text bls) ++ [nl]
    clearLayout = clearLayoutT
    moveToNextLine = moveToNextLineT
    ringBell True = output bellAudible
    ringBell False = output bellVisual
