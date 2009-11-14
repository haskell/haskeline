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
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Concurrent.Chan

import System.Console.Haskeline.Monads as Monads
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Term
import System.Console.Haskeline.Backend.Posix
import System.Console.Haskeline.Key

----------------------------------------------------------------
-- Low-level terminal output

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
    -- This capability is not strictly necessary, but is very widely supported
    -- and assuming it makes for a much simpler implementation of printText.
    autoRightMargin >>= guard

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
    wrapLine' <- getWrapLine (leftA' 1)
    return Actions{leftA = leftA', rightA = rightA',upA = upA',
                clearToLineEnd = clearToLineEnd', nl = nl',cr = cr',
                bellAudible = bellAudible', bellVisual = bellVisual',
                clearAllA = clearAll',
                 wrapLine = wrapLine'}

-- If the wraparound glitch is in effect, force a wrap by printing a space.
-- Otherwise, it'll wrap automatically.
getWrapLine :: TermOutput -> Capability TermOutput
getWrapLine left1 = (do
    wraparoundGlitch >>= guard
    return (termText " " <#> left1)
    ) `mplus` return mempty

type TermAction = Actions -> TermOutput
    
text :: B.ByteString -> TermAction
text str _ = termText $ B.unpack str

left,right,up :: Int -> TermAction
left = flip leftA
right = flip rightA
up = flip upA

clearAll :: LinesAffected -> TermAction
clearAll = flip clearAllA

mreplicate :: Monoid m => Int -> m -> m
mreplicate n m
    | n <= 0    = mempty
    | otherwise = m `mappend` mreplicate (n-1) m


----------------------------------------------------------------
-- The Draw monad

-- denote in modular arithmetic;
-- in particular, 0 <= termCol < width
data TermPos = TermPos {termRow,termCol :: !Int}
    deriving Show

initTermPos :: TermPos
initTermPos = TermPos {termRow = 0, termCol = 0}



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
    maybeOutput = liftIO . hRunTermOutput h term .
                            fromMaybe mempty . getCapability term

tinfoLayout :: Terminal -> IO (Maybe Layout)
tinfoLayout term = return $ getCapability term $ do
                        r <- termColumns
                        c <- termLines
                        return Layout {height=r,width=c}

terminfoKeys :: Terminal -> [(String,Key)]
terminfoKeys term = mapMaybe getSequence keyCapabilities
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


----------------------------------------------------------------
-- Movement actions

changePos :: TermPos -> TermPos -> TermAction
changePos TermPos {termRow=r1, termCol=c1} TermPos {termRow=r2, termCol=c2}
    | r1 == r2 = if c1 < c2 then right (c2-c1) else left (c1-c2)
    | r1 > r2 = cr <#> up (r1-r2) <#> right c2
    | otherwise = cr <#> mreplicate (r2-r1) nl <#> right c2

moveToPos :: TermPos -> DrawM ()
moveToPos p = do
    oldP <- get
    output (changePos oldP p)
    put p

moveRelative :: Int -> DrawM ()
moveRelative n = do
    p <- get
    l <- ask
    moveToPos $ advancePos l n p

changeRight, changeLeft :: Int -> DrawM ()
changeRight n   | n <= 0 = return ()
                | otherwise = moveRelative n
changeLeft n    | n <= 0 = return ()
                | otherwise = moveRelative (negate n)

advancePos :: Layout -> Int -> TermPos -> TermPos
advancePos Layout {width=w} k p 
    = TermPos {termRow = n `div` w, termCol = n `mod` w}
  where
    n = k + w * termRow p + termCol p

----------------------------------------------------------------
-- Text printing actions

printText :: [Grapheme] -> DrawM ()
printText [] = return ()
printText gs = do
    l <- ask
    modify $ advancePos l (length gs)
    strOutput <- posixEncode $ graphemesToString gs
    wrap <- gets $ \p -> if termCol p==0 then wrapLine else mempty
    output (text strOutput <#> wrap)

----------------------------------------------------------------
-- High-level Term implementation

drawLineDiffT :: LineChars -> LineChars -> DrawM ()
drawLineDiffT (xs1,ys1) (xs2,ys2) = case matchInit xs1 xs2 of
    ([],[])     | ys1 == ys2            -> return ()
    (xs1',[])   | xs1' ++ ys1 == ys2    -> changeLeft (length xs1')
    ([],xs2')   | ys1 == xs2' ++ ys2    -> changeRight (length xs2')
    (xs1',xs2')                         -> do
        changeLeft (length xs1')
        -- TODO: is splitting up stuff bad?
        -- Maybe it'd be better if there was some sort of flushing...
        -- or we could just concat all of the TermActions together...
        printText xs2'
        p <- get
        printText ys2
        -- TODO: maybe better to precompute before starting to output?
        -- hard to say...
        clearDeadText $ length xs1' + length ys1 - (length xs2' + length ys2)
        moveToPos p 

linesLeft :: Layout -> TermPos -> Int -> Int
linesLeft Layout {width=w} TermPos {termCol = c} n
    | c + n < w = 1
    | otherwise = 1 + div (c+n) w

lsLinesLeft :: Layout -> TermPos -> LineChars -> Int
lsLinesLeft layout pos = linesLeft layout pos . lengthToEnd

clearDeadText :: Int -> DrawM ()
clearDeadText n
    | n <= 0    = return ()
    | otherwise = do
        layout <- ask
        pos <- get
        let extraLines = linesLeft layout pos n - 1
        output clearToLineEnd -- don't always do this?
        when (extraLines > 0) $ do
            output $ mreplicate extraLines
                            $ nl <#> clearToLineEnd
            put TermPos {termRow = termRow pos + extraLines, termCol=0}

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
