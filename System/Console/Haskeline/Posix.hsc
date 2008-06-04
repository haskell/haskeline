module System.Console.Haskeline.Posix (
                        withGetEvent,
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

import System.Console.Haskeline.Monads
import System.Console.Haskeline.Command

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

withGetEvent :: (MonadReader Terminal m, MonadIO m) 
                => Bool -> (m Event -> m a) -> m a
withGetEvent useSigINT f = do
    term <- ask
    eventChan <- liftIO $ newTChanIO
    waitingForKey <- liftIO $ newTVarIO False
    wrapKeypad term 
        $ withWindowHandler eventChan
        $ withSigIntHandler useSigINT eventChan
      $ withForked (commandLoop term eventChan waitingForKey)
      $ f $ readEvent eventChan waitingForKey
  where
    wrapKeypad term f = finallyIO (liftIO (maybeOutput term keypadOn) >> f)
                        (maybeOutput term keypadOff)
    maybeOutput term cap = runTermOutput term $ 
            fromMaybe mempty (getCapability term cap)
    readEvent eventChan waitingForKey = liftIO $ do
        atomically $ writeTVar waitingForKey True
        atomically $ readTChan eventChan

withWindowHandler :: MonadIO m => TChan Event -> m a -> m a
withWindowHandler eventChan f = do
    let handler = getLayout >>= atomically . writeTChan eventChan . WindowResize
    old_handler <- liftIO $ installHandler windowChange (Catch handler) Nothing
    f `finallyIO` installHandler windowChange old_handler Nothing

withSigIntHandler :: MonadIO m => Bool -> TChan Event -> m a -> m a
withSigIntHandler False _ f = f
withSigIntHandler True eventChan f = do
    let handler = atomically $ writeTChan eventChan SigInt
    old_handler <- liftIO $ installHandler sigINT (CatchOnce handler) Nothing
    f `finallyIO` installHandler sigINT old_handler Nothing

-- fork a thread, then kill it after the computation is done
withForked :: MonadIO m => IO () -> m a -> m a
withForked threadAct f = do
    threadID <- liftIO $ forkIO threadAct
    f `finallyIO` killThread threadID
    
commandLoop :: Terminal -> TChan Event -> TVar Bool -> IO ()
commandLoop term eventChan waitingForKey = do
    keySeqs <- getKeySequences term
    let loop = do
        atomically $ readTVar waitingForKey >>= guard >> writeTVar waitingForKey False
        k <- getKey keySeqs
        atomically $ writeTChan eventChan (KeyInput k)
        loop
    loop
