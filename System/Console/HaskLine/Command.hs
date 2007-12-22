module System.Console.HaskLine.Command where

import System.Console.HaskLine.LineState
import System.Console.Terminfo
import Data.Maybe
import Data.List

import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import System.Console.Terminfo
import System.Posix (Fd,stdOutput)
import System.Posix.Terminal
import System.Timeout
import Control.Monad
import Control.Monad.Trans

import Data.Bits

data Key = KeyChar Char | KeyMeta Char
            | KeyLeft | KeyRight | KeyUp | KeyDown
            | Backspace | DeleteForward | KillLine
                deriving (Eq,Ord,Show)

-- Easy translation of control characters; e.g., Ctrl-G or Ctrl-g or ^G
controlKey :: Char -> Key
controlKey '?' = KeyChar (toEnum 127)
controlKey c = KeyChar $ toEnum $ fromEnum c .&. complement (bit 5 .|. bit 6)

getKeySequences :: Terminal -> IO (TreeMap Char Key)
getKeySequences term = do
    sttys <- sttyKeys
    let tinfos = terminfoKeys term
    let chars = map (\c -> ([c],KeyChar c)) $ map toEnum [0..127]
    let metas = map (\c -> (['\ESC',c],KeyMeta c)) $ map toEnum [0..127]
    -- note ++ acts as a union; so the below favors sttys over chars
    return $ listToTree $ chars ++ metas ++ tinfos ++ sttys


terminfoKeys :: Terminal -> [(String, Key)]
terminfoKeys term = catMaybes $ map getSequence keyCapabilities
        where getSequence (cap,x) = getCapability term $ do 
                            keys <- cap
                            return (keys,x)

keyCapabilities = [(keyLeft,KeyLeft),
                (keyRight,KeyRight),
                (keyUp,KeyUp),
                (keyDown,KeyDown),
                (keyBackspace,Backspace),
                (keyDeleteChar,DeleteForward)]

sttyKeys :: IO [(String, Key)]
sttyKeys = do
    attrs <- getTerminalAttributes stdOutput
    let getStty (k,c) = do {str <- controlChar attrs k; return ([str],c)}
    return $ catMaybes $ map getStty [(Erase,Backspace),(Kill,KillLine)]

getKey :: TreeMap Char Key -> IO Key
getKey baseMap = getChar >>= getKey' baseMap
    where
        getKey' (TreeMap tm) c = case Map.lookup c tm of
            Nothing -> getKey baseMap -- unrecognized control sequence; try again.
            Just (Nothing,t) -> getChar >>= getKey' t
            Just (Just k,t@(TreeMap tm2))
                | Map.null tm2 -> return k
                | otherwise  -> do
                -- We have a choice of either accepting the current sequence, 
                -- or reading more characters to form a longer sequence.
                    md <- timeout escDelay getChar
                    case md of
                        Nothing -> return k
                        Just d -> getKey' t d

newtype TreeMap a b = TreeMap (Map.Map a (Maybe b, TreeMap a b))
                        deriving Show

emptyTreeMap :: TreeMap a b
emptyTreeMap = TreeMap Map.empty

insertIntoTree :: Ord a => ([a], b) -> TreeMap a b -> TreeMap a b
insertIntoTree ([],_) _ = error "Can't insert empty list into a treemap!"
insertIntoTree ((c:cs),k) (TreeMap m) = TreeMap (Map.alter f c m)
    where
        alterSubtree = insertIntoTree (cs,k)
        f Nothing = Just $ if null cs
                            then (Just k, emptyTreeMap)
                            else (Nothing, alterSubtree emptyTreeMap)
        f (Just (y,t)) = Just $ if null cs
                                    then (Just k, t)
                                    else (y, alterSubtree t)

listToTree :: Ord a => [([a],b)] -> TreeMap a b
listToTree = foldl' (flip insertIntoTree) emptyTreeMap

mapLines :: (Show a, Show b) => TreeMap a b -> [String]
mapLines (TreeMap m) = let
    m2 = Map.map (\(k,t) -> show k : mapLines t) m
    in concatMap (\(k,ls) -> show k : map (' ':) ls) $ Map.toList m2


-- Time, in microseconds, to wait before timing out and reading e.g. an escape
-- as one character instead of as part of a control sequence.
escDelay :: Int
escDelay = 100000 -- 0.1 seconds


        
{-- todo: some commands only change the linestate, don't require
 a full refresh:
 data Command m = Change (LSCHANGE) | Refresh (Linestate -> m LineState)
 --}

data Command m = Finish | Command (LineState -> m Result)
                | RedrawLine {shouldClearScreen :: Bool}

data Result = Changed LineState | PrintLines (Layout -> [String]) LineState
                                   


isFinish :: Command m -> Bool
isFinish Finish = True
isFinish _ = False

type Commands m = Map.Map Key (Command m)

simpleCommands :: Monad m => Commands m
simpleCommands = Map.fromList $ [
                    (KeyChar '\n', Finish)
                    ,(KeyLeft, pureCommand goLeft)
                    ,(KeyRight, pureCommand goRight)
                    ,(Backspace, pureCommand deletePrev)
                    ,(DeleteForward, pureCommand deleteNext)
                    -- ,(KillLine, pureCommand killLine)
                    ] ++ map insertionCommand [' '..'~']
            
pureCommand :: Monad m => LineChange -> Command m
pureCommand f = Command (return . Changed . f)

insertionCommand :: Monad m => Char -> (Key,Command m)
insertionCommand c = (KeyChar c, pureCommand $ insertChar c)

changeCommand :: Monad m => (LineState -> m LineState) -> Command m
changeCommand f = Command $ \ls -> liftM Changed (f ls)
                    
newtype CommandT s m a = CommandT {runCommandT :: s -> m (a,s)}

evalCommandT :: Monad m => s -> CommandT s m a -> m a
evalCommandT s f = liftM fst $ runCommandT f s

instance Monad m => Monad (CommandT s m) where
    return x = CommandT $ \s -> return (x,s)
    CommandT f >>= g = CommandT $ \s -> do
                                (x,s') <- f s
                                runCommandT (g x) s'

instance MonadTrans (CommandT s) where
    lift f = CommandT $ \s -> do {x <- f; return (x,s)}

instance MonadIO m => MonadIO (CommandT s m) where
    liftIO f = CommandT $ \s -> do {x <- liftIO f; return (x,s)}

class MonadIO m => MonadIO1 m where
    liftIO1 :: (forall a . IO a -> IO a) -> m a -> m a

instance MonadIO1 IO where
    liftIO1 = id

instance MonadIO1 m => MonadIO1 (CommandT s m) where
    liftIO1 f (CommandT g) = CommandT $ liftIO1 f . g
    
class Monad m => MonadCmd s m where
    getState :: m s
    putState :: s -> m ()

instance Monad m => MonadCmd s (CommandT s m) where
    getState = CommandT $ \s -> return (s,s)
    putState s = CommandT $ \_ -> return ((),s)

instance MonadCmd s m => MonadCmd s (CommandT t m) where
    getState = lift getState
    putState s = lift (putState s)

-- A useful combinator
updateState :: MonadCmd s m => (s -> (a,s)) -> m a
updateState f = do
    s <- getState
    let (x,s') = f s
    putState s'
    return x

modifyState :: MonadCmd s m => (s -> s) -> m ()
modifyState f = getState >>= putState . f

