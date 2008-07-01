module System.Console.Haskeline.Backend.Posix (
                        withPosixGetEvent,
                        getPosixLayout,
                        mapLines
                 ) where

import Foreign
import Foreign.C.Types
import qualified Data.Map as Map
import System.Console.Terminfo
import System.Posix (stdOutput)
import System.Posix.Terminal
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import System.Posix.Signals.Exts
import System.Posix.IO(stdInput)
import Data.List
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8

import System.Console.Haskeline.Monads
import System.Console.Haskeline.Command

#include <sys/ioctl.h>

-------------------
-- Window size

foreign import ccall ioctl :: CInt -> CULong -> Ptr a -> IO ()

getPosixLayout :: IO Layout
getPosixLayout = allocaBytes (#size struct winsize) $ \ws -> do
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

-- TODO: What if term not found?
getKeySequences :: Maybe Terminal -> IO (TreeMap Char Key)
getKeySequences term = do
    sttys <- sttyKeys
    let tinfos = fromMaybe ansiKeys (term >>= terminfoKeys)
    -- note ++ acts as a union; so the below favors sttys over tinfos
    return $ listToTree $ tinfos ++ sttys


ansiKeys :: [(String, Key)]
ansiKeys = [("\ESC[D",  KeyLeft)
            ,("\ESC[C",  KeyRight)
            ,("\ESC[A",  KeyUp)
            ,("\ESC[B",  KeyDown)
            ,("\b",      Backspace)]

terminfoKeys :: Terminal -> Maybe [(String,Key)]
terminfoKeys term = getCapability term $ mapM getSequence keyCapabilities
    where 
        getSequence (cap,x) = do 
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

-- for debugging '
mapLines :: (Show a, Show b) => TreeMap a b -> [String]
mapLines (TreeMap m) = let
    m2 = Map.map (\(k,t) -> show k : mapLines t) m
    in concatMap (\(k,ls) -> show k : map (' ':) ls) $ Map.toList m2

lexKeys :: TreeMap Char Key -> [Char] -> [Key]
lexKeys _ [] = []
lexKeys baseMap cs
    | Just (k,ds) <- lookupChars baseMap cs
            = k : lexKeys baseMap ds
lexKeys baseMap ('\ESC':c:cs) = KeyMeta c : lexKeys baseMap cs
lexKeys baseMap (c:cs) = KeyChar c : lexKeys baseMap cs

lookupChars :: TreeMap Char Key -> [Char] -> Maybe (Key,[Char])
lookupChars _ [] = Nothing
lookupChars (TreeMap tm) (c:cs) = case Map.lookup c tm of
    Nothing -> Nothing
    Just (Nothing,t) -> lookupChars t cs
    Just (Just k, t@(TreeMap tm2))
                | not (null cs) && not (Map.null tm2) -- ?? lookup d tm2?
                    -> lookupChars t cs
                | otherwise -> Just (k, cs)

-----------------------------

withPosixGetEvent :: MonadException m => Maybe Terminal -> Bool -> (m Event -> m a) -> m a
withPosixGetEvent term useSigINT f = do
    baseMap <- liftIO (getKeySequences term)
    eventChan <- liftIO $ newTChanIO
    wrapKeypad term 
        $ withWindowHandler eventChan
        $ withSigIntHandler useSigINT eventChan
        $ f $ liftIO $ getEvent eventChan baseMap

-- If the keypad on/off capabilities are defined, wrap the computation with them.
wrapKeypad :: MonadException m => Maybe Terminal -> m a -> m a
wrapKeypad Nothing f = f
wrapKeypad (Just term) f = (maybeOutput keypadOn >> f) 
                            `finally` maybeOutput keypadOn
  where
    maybeOutput cap = liftIO $ runTermOutput term $
                            fromMaybe mempty (getCapability term cap)

withWindowHandler :: MonadException m => TChan Event -> m a -> m a
withWindowHandler eventChan = withHandler windowChange $ Catch $
    getPosixLayout >>= atomically . writeTChan eventChan . WindowResize

withSigIntHandler :: MonadException m => Bool -> TChan Event -> m a -> m a
withSigIntHandler False _ = id
withSigIntHandler True eventChan = withHandler keyboardSignal $ CatchOnce $
            atomically $ writeTChan eventChan SigInt

withHandler :: MonadException m => Signal -> Handler -> m a -> m a
withHandler signal handler f = do
    old_handler <- liftIO $ installHandler signal handler Nothing
    f `finally` liftIO (installHandler signal old_handler Nothing)

getEvent :: TChan Event -> TreeMap Char Key -> IO Event
getEvent eventChan baseMap = do
    -- first, see if any events are already queued up (from a key/ctrl-c
    -- event or from a previous call to getEvent where we read in multiple
    -- keys)
    me <- atomically $ tryReadTChan eventChan
    case me of
        Just e -> return e
        Nothing -> do
            -- no events are queued yet, so fork off a thread to read keys.
            -- if we receive a different type of event before it's done,
            -- we'll kill it.
            tid <- forkIO readKeyEvents
            e <- atomically $ readTChan eventChan -- key or other event
            killThread tid
            return e
  where
    bufferSize = 100
    readKeyEvents = do
        -- Read at least one character of input, and more if available.
        -- In particular, the characters making up a control sequence will all
        -- be available at once, so we can process them together with lexKeys.
        threadWaitRead stdInput
        bs <- B.hGetNonBlocking stdin bufferSize
        let cs = UTF8.toString bs
        let ks = map KeyInput $ lexKeys baseMap cs
        if null ks
            then readKeyEvents
            else atomically $ mapM_ (writeTChan eventChan) ks

tryReadTChan :: TChan a -> STM (Maybe a)
tryReadTChan chan = fmap Just (readTChan chan) `orElse` return Nothing
