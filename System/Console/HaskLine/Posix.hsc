module System.Console.HaskLine.Posix (
                        withGetEvent,
                        Event(..),
                        Key(..),
                        getLayout,
                        mapLines
                 ) where

import Foreign
import Foreign.C.Types
import qualified Data.Map as Map
import System.Console.Terminfo
import System.Posix (stdOutput)
import System.Posix.Terminal
import System.Timeout
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import System.Posix.Signals.Exts
import Data.List

import System.Console.HaskLine.Draw
import System.Console.HaskLine.Monads

#include <sys/ioctl.h>


-------------------
-- Window size
foreign import ccall ioctl :: CInt -> CULong -> Ptr a -> IO ()

getLayout :: IO Layout
getLayout = allocaBytes (#size struct winsize) $ \ws -> do
                            ioctl 1 (#const TIOCGWINSZ) ws
                            rows :: CUShort <- (#peek struct winsize,ws_row) ws
                            cols :: CUShort <- (#peek struct winsize,ws_col) ws
                            return Layout {height=fromEnum rows,width=fromEnum cols}

-- todo: make sure >=2
-- TODO: other ways of getting it:
-- env vars ROWS/COLUMNS
-- terminfo capabilities


--------------------
-- Key sequences
data Key = KeyChar Char | KeyMeta Char
            | KeyLeft | KeyRight | KeyUp | KeyDown
            | Backspace | DeleteForward | KillLine
                deriving (Eq,Ord,Show)

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

---- '------------------------

data Event = WindowResize Layout | KeyInput Key

withGetEvent :: MonadIO m => Terminal -> (m Event -> m a) -> m a
withGetEvent term f = do
    chan <- liftIO $ atomically $ newTChan
    withWindowHandler chan $ withForked (commandLoop term chan)
        $ f (liftIO $ atomically $ readTChan chan)

withWindowHandler :: MonadIO m => TChan Event -> m a -> m a
withWindowHandler tv f = do
    let handler = getLayout >>= atomically . writeTChan tv . WindowResize
    old_handler <- liftIO $ installHandler windowChange (Catch handler) Nothing
    f `finallyIO` installHandler windowChange old_handler Nothing

-- fork a thread, then kill it after the computation is done
withForked :: MonadIO m => IO () -> m a -> m a
withForked threadAct f = do
    threadID <- liftIO $ forkIO threadAct
    f `finallyIO` killThread threadID
    
commandLoop :: Terminal -> TChan Event -> IO ()
commandLoop term tv = do
    keySeqs <- getKeySequences term
    let loop = do
        k <- getKey keySeqs
        atomically $ writeTChan tv (KeyInput k) 
        loop
    loop

