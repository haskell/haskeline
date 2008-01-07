module System.Console.HaskLine.Command where

import System.Console.HaskLine.LineState
import System.Console.Terminfo
import Data.Maybe
import Data.List

import qualified Data.Map as Map
import System.Console.Terminfo
import System.Posix (stdOutput)
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
              keyCapabilities = 
                [(keyLeft,KeyLeft),
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



--------------------------------
data Effect s = Change s | PrintLines (Layout -> [String]) s
                | Redraw {shouldClearScreen :: Bool, redrawState :: s}
                | Fail | Finish

newtype KeyMap m s = KeyMap {keyMap :: Map.Map Key (KeyAction m s)}

data KeyAction m s = forall t . LineState t 
                    => KeyAction (s -> m (Effect t)) (KeyMap m t)

lookupKM :: KeyMap m s -> Key -> Maybe (KeyAction m s)
lookupKM km k = Map.lookup k (keyMap km)

nullKM :: KeyMap m s
nullKM = KeyMap $ Map.empty

orKM :: KeyMap m s -> KeyMap m s -> KeyMap m s
orKM (KeyMap m) (KeyMap n) = KeyMap $ m `Map.union` n

choiceKM :: [KeyMap m s] -> KeyMap m s
choiceKM = foldl orKM nullKM

nullAction :: (LineState s, Monad m) => KeyMap m s -> KeyAction m s
nullAction = KeyAction (return . Change . id)

acceptKey :: (Monad m) => Key -> KeyAction m s -> KeyMap m s
acceptKey k act = KeyMap $ Map.singleton k act

acceptGraph :: (Char -> KeyAction m s) -> KeyMap m s
acceptGraph f = KeyMap $ Map.fromList 
                $ map (\c -> (KeyChar c, f c)) [' '..'~']
                         

type Command m s t = KeyMap m t -> KeyMap m s

finish :: forall s m t . (LineState t, Monad m) => Key -> Command m s t
finish k _ = acceptKey k $ KeyAction (\_ -> return (Finish :: Effect t)) nullKM

simpleCommand :: (LineState t, Monad m) => (s -> m (Effect t)) 
                    -> Key -> Command m s t
simpleCommand f = \k next -> acceptKey k $ KeyAction f next

changeCommand :: (LineState t, Monad m) => (s -> t) -> Key -> Command m s t
changeCommand f = simpleCommand (return . Change . f)

(+>) :: Key -> (Key -> a) -> a 
k +> f = f k

choiceCmd :: KeyMap m t -> [Command m s t] -> KeyMap m s
choiceCmd next cmds = choiceKM $ map ($ next) cmds

graphCommand :: (Monad m, LineState t) => (Char -> s -> t) -> Command m s t
graphCommand f next = acceptGraph $ \c -> KeyAction (return . Change . f c) next

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
    liftIO1 :: (forall b . IO b -> IO b) -> m a -> m a

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

