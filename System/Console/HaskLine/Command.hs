module System.Console.HaskLine.Command(
                        -- * Key Sequences
                        Key(..),
                        controlKey,
                        getKeySequences,
                        getKey,
                        TreeMap(),
                        emptyTreeMap,
                        insertIntoTree,
                        listToTree,
                        mapLines,
                        -- * Commands
                        Effect(..),
                        Layout(..),
                        KeyMap(), 
                        lookupKM,
                        KeyAction(..),
                        Command(),
                        runCommand,
                        continue,
                        (>|>),
                        (>+>),
                        acceptKey,
                        acceptKeyM,
                        acceptChar,
                        loopUntil,
                        loopWithBreak,
                        try,
                        finish,
                        simpleCommand,
                        change,
                        changeWithoutKey,
                        clearScreenCmd,
                        (+>),
                        choiceCmd,
                        spliceCmd
                        ) where

import System.Console.HaskLine.LineState
import System.Console.HaskLine.Draw
import System.Console.Terminfo
import Data.Maybe
import Data.List

import qualified Data.Map as Map
import System.Console.Terminfo
import System.Posix (stdOutput)
import System.Posix.Terminal
import System.Timeout
import Control.Monad

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

-- for debugging
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

newtype KeyMap m s = KeyMap {keyMap :: Map.Map Key (s -> m (KeyAction m))}

data KeyAction m = forall t . LineState t => KeyAction (Effect t) (KeyMap m t)

lookupKM :: KeyMap m s -> Key -> Maybe (s -> m (KeyAction m))
lookupKM km k = Map.lookup k (keyMap km)

nullKM :: KeyMap m s
nullKM = KeyMap $ Map.empty

orKM :: KeyMap m s -> KeyMap m s -> KeyMap m s
orKM (KeyMap m) (KeyMap n) = KeyMap $ m `Map.union` n

choiceKM :: [KeyMap m s] -> KeyMap m s
choiceKM = foldl orKM nullKM

nullAction :: (LineState s, Monad m) => s -> m (Effect s)
nullAction = return . Change

newtype Command m s t = Command (KeyMap m t -> KeyMap m s)

runCommand :: Command m s s -> KeyMap m s
runCommand (Command f) = let m = f m in m

continue :: Command m s s
continue = Command id

infixl 6 >|>
(>|>) :: Command m s t -> Command m t u -> Command m s u
Command f >|> Command g = Command (f . g)

infixl 6 >+>
(>+>) :: (Monad m, LineState s) => Key -> Command m s t -> Command m s t
k >+> f = k +> change id >|> f

acceptKey :: (Monad m, LineState t) => 
                Key -> (s -> m (Effect t)) -> Command m s t
acceptKey k act = Command $ \next -> KeyMap $ Map.singleton k $ \s -> do 
                                                t <- act s
                                                return (KeyAction t next)

acceptChar :: (Monad m, LineState t) => (Char -> s -> t) -> Command m s t
acceptChar f = Command $ \next -> KeyMap $ Map.fromList 
                $ map (\c -> (KeyChar c,\s -> return (KeyAction (Change (f c s)) next)))
                    [' '..'~']

acceptKeyM :: (Monad m, LineState s) => Key -> (s -> m (Effect s, Command m s s))
                            -> Command m s s
acceptKeyM k f = Command $ \next -> KeyMap $ Map.singleton k $ \s -> do
                (effect,Command g) <- f s
                return (KeyAction effect (g next))
                         
loopUntil :: Command m s s -> Command m s t -> Command m s t
loopUntil (Command f) (Command g) 
    = Command $ \next -> let loop = g next `orKM` f loop
                         in loop

loopWithBreak :: Command m s s -> Command m s t -> (s -> t) -> Command m s t
loopWithBreak cmd end f = Command $ \next -> 
    runCommand $ choiceCmd [cmd, spliceCmd next end, 
                        spliceCmd next (changeWithoutKey f)]

try :: Command m s s -> Command m s s
try (Command f) = Command $ \next -> (f next) `orKM` next

finish :: forall s m t . (LineState t, Monad m) => Key -> Command m s t
finish k = acceptKey k $ const (return Finish)

-- NOTE: SAME AS ACCEPTKEY
simpleCommand :: (LineState t, Monad m) => (s -> m (Effect t)) 
                    -> Key -> Command m s t
simpleCommand f k = acceptKey k f

change :: (LineState t, Monad m) => (s -> t) -> Key -> Command m s t
change f = simpleCommand (return . Change . f)

changeWithoutKey :: (s -> t) -> Command m s t
changeWithoutKey f = Command $ \(KeyMap next) -> KeyMap $ fmap (\g -> g . f) next
    -- (\(KeyAction g next') -> KeyAction (g . f) next') next

clearScreenCmd :: (LineState s, Monad m) => Key -> Command m s s
clearScreenCmd k = acceptKey k $ \s -> return (Redraw True s)

infixl 7 +>
(+>) :: Key -> (Key -> a) -> a 
k +> f = f k

choiceCmd :: [Command m s t] -> Command m s t
choiceCmd cmds = Command $ \next -> 
    choiceKM $ map (\(Command f) -> f next) cmds

spliceCmd :: KeyMap m t -> Command m s t -> Command m s u
spliceCmd alternate (Command f) = Command $ \_-> f alternate


