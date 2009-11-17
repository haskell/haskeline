module System.Console.Haskeline.Backend.Terminfo(
                            Draw(),
                            runTerminfoDraw
                            )
                             where

import System.Console.Terminfo
import Control.Monad
import Data.List(intersperse, foldl')
import System.IO
import qualified Control.Exception.Extensible as Exception
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Concurrent.Chan
import qualified Data.IntMap as Map

import System.Console.Haskeline.Monads as Monads
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Term
import System.Console.Haskeline.Backend.Posix
import System.Console.Haskeline.Backend.WCWidth
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

-- We don't need to bother encoding the spaces.
spaces :: Int -> TermAction
spaces 0 = mempty
spaces 1 = const $ termText " " -- share when possible
spaces n = const $ termText $ replicate n ' '

----------------------------------------------------------------
-- The Draw monad

-- denote in modular arithmetic;
-- in particular, 0 <= termCol < width
data TermPos = TermPos {termRow,termCol :: !Int}
    deriving Show

initTermPos :: TermPos
initTermPos = TermPos {termRow = 0, termCol = 0}

-- IDEA: instead of row lengths, keep track on index of first char of the row.
data TermRows = TermRows {rowLengths :: !(Map.IntMap Int), lastRow :: !Int}
    deriving Show

initTermRows :: TermRows
initTermRows = TermRows {rowLengths = Map.singleton 0 0, lastRow=0}

newtype Draw m a = Draw {unDraw :: (ReaderT Actions
                                    (ReaderT Terminal
                                    (StateT TermRows
                                    (StateT TermPos
                                    (PosixT m))))) a}
    deriving (Monad, MonadIO, MonadException,
              MonadReader Actions, MonadReader Terminal, MonadState TermPos,
              MonadState TermRows,
              MonadReader Handle, MonadReader Encoders)

type DrawM a = forall m . (MonadReader Layout m, MonadIO m) => Draw m a

instance MonadTrans Draw where
    lift = Draw . lift . lift . lift . lift . lift . lift
    
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
                              $ evalStateT' initTermRows
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

-- TODO: when drawLineDiffT calls this, shouldn't move if same.
moveToPos :: TermPos -> DrawM TermAction
moveToPos p = do
    oldP <- get
    put p
    return $ changePos oldP p

moveRelative :: Int -> DrawM ()
moveRelative n = liftM3 (advancePos n) ask get get
                    >>= moveToPos >>= output

-- Note that these move by a certain number of cells, not graphemes.
changeRight, changeLeft :: Int -> DrawM ()
changeRight n   | n <= 0 = return ()
                | otherwise = moveRelative n
changeLeft n    | n <= 0 = return ()
                | otherwise = moveRelative (negate n)

-- TODO: this could be more efficient by only checking intermediate rows.
-- TODO: this is worth handling with QuickCheck.
-- TODO: intermediates checking would be simpler if each row was given the
-- index of col 0.
advancePos :: Int -> Layout -> TermRows -> TermPos -> TermPos
advancePos k Layout {width=w} rs p = indexToPos $ k + posIndex
  where
    posIndex = termCol p + sum' (map (lookupCells rs)
                                            [0..termRow p-1])
    indexToPos n = loopFindRow 0 n
    loopFindRow r m = r `seq` m `seq` let
        thisRowSize = lookupCells rs r
        in if m < thisRowSize
                || (m == thisRowSize && m < w)
                || thisRowSize <= 0 -- This shouldn't happen in practice,
                                    -- but double-check to prevent an infinite loop
                then TermPos {termRow=r, termCol=m}
                else loopFindRow (r+1) (m-thisRowSize)

lookupCells :: TermRows -> Int -> Int
lookupCells (TermRows rc _) r = Map.findWithDefault 0 r rc

sum' :: [Int] -> Int
sum' = foldl' (+) 0

----------------------------------------------------------------
-- Text printing actions


setRow :: Int -> Int -> TermRows -> TermRows
setRow r len rs = TermRows {rowLengths = Map.insert r len (rowLengths rs),
                            lastRow=r}

encodeGraphemes :: MonadIO m => [Grapheme] -> Draw m TermAction
encodeGraphemes = liftM text . posixEncode . graphemesToString

printText :: [Grapheme] -> DrawM TermAction
printText gs = do
    act <- textAction mempty gs
    -- Work around some corner cases...
    -- TODO: this isn't always necessary.
    get >>= \p -> modify $ setRow (termRow p) (termCol p)
    return act

-- To prevent flicker, we queue all text into one big TermAction and then
-- output it all at once.
textAction :: TermAction -> [Grapheme] -> DrawM TermAction
textAction prevOutput [] = return prevOutput
textAction prevOutput gs = do
    -- First, get the monadic parameters:
    w <- asks width
    TermPos {termRow=r, termCol=c} <- get
    -- Now, split the line
    let (thisLine,rest,thisWidth) = splitAtWidth (w-c) gs
    let lineWidth = c + thisWidth
    modify $ setRow r lineWidth
    ts <- encodeGraphemes thisLine
    -- Finally, actually print out the relevant text.
    if null rest && lineWidth < w
        then do -- everything fits on one line without wrapping
            put TermPos {termRow=r, termCol=lineWidth}
            return (prevOutput <#> ts)
        else do -- Must wrap to the next line
            put TermPos {termRow=r+1,termCol=0}
            let wrap = if lineWidth == w then wrapLine else spaces (w-lineWidth)
            textAction (prevOutput <#> ts <#> wrap) rest

----------------------------------------------------------------
-- High-level Term implementation

drawLineDiffT :: LineChars -> LineChars -> DrawM ()
drawLineDiffT (xs1,ys1) (xs2,ys2) = case matchInit xs1 xs2 of
    ([],[])     | ys1 == ys2            -> return ()
    (xs1',[])   | xs1' ++ ys1 == ys2    -> changeLeft (gsWidth xs1')
    ([],xs2')   | ys1 == xs2' ++ ys2    -> changeRight (gsWidth xs2')
    (xs1',xs2')                         -> do
        oldRS <- get
        -- TODO: Merge this with the rest of the output.  However, it's not as
        -- important since when typing new text xs1' is empty.
        changeLeft (gsWidth xs1')
        xsOut <- printText xs2'
        p <- get
        restOut <- liftM mconcat $ sequence
                        [ printText ys2
                        , get >>= clearDeadText oldRS
                        , moveToPos p
                        ]
        output (xsOut <#> restOut)

-- the number of lines after this one.
getLinesLeft :: DrawM Int
getLinesLeft = do
    p <- get
    rc <- get
    return (lastRow rc - termRow p)

clearDeadText :: TermRows -> TermRows -> DrawM TermAction
clearDeadText oldRS newRS
    | extraRows < 0
        || (extraRows == 0 && lookupCells oldRS r <= lookupCells newRS r)
            = return mempty
    | otherwise = do
        when (extraRows /= 0) $ do
            p <- get
            put TermPos {termRow = termRow p + extraRows, termCol=0}
        return $ clearToLineEnd <#> mreplicate extraRows (nl <#> clearToLineEnd)
  where
    extraRows = lastRow oldRS - lastRow newRS
    r = lastRow newRS

clearLayoutT :: DrawM ()
clearLayoutT = do
    h <- asks height
    output (clearAll h)
    put initTermPos

moveToNextLineT :: LineChars -> DrawM ()
moveToNextLineT _ = do
    lleft <- getLinesLeft
    output $ mreplicate (lleft+1) nl
    put initTermPos
    put initTermRows

repositionT :: Layout -> LineChars -> DrawM ()
repositionT _ s = do
    oldPos <- get
    l <- getLinesLeft
    output $ cr <#> mreplicate l nl
            <#> mreplicate (l + termRow oldPos) (clearToLineEnd <#> up 1)
    put initTermPos
    put initTermRows
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
