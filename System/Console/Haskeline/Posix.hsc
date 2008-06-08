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
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import System.Posix.Signals.Exts
import Data.List
import System.IO

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

lexKeys :: TreeMap Char Key -> [Char] -> [Key]
lexKeys baseMap = loop baseMap
    where
        loop _ [] = []
        loop (TreeMap tm) (c:cs) = case Map.lookup c tm of
            Nothing -> loop baseMap cs
            Just (Nothing,t) -> loop t cs
            Just (Just k,t@(TreeMap tm2))
                | not (null cs) && not (Map.null tm2) -- ?? lookup d tm2?
                    -> loop t cs
                    | otherwise -> k : loop baseMap cs

---- '------------------------

withGetEvent :: (MonadReader Terminal m, MonadIO m) 
                => Bool -> (m Event -> m a) -> m a
withGetEvent useSigINT f = do
    term <- ask
    baseMap <- liftIO (getKeySequences term)
    eventChan <- liftIO $ newTChanIO
    wrapKeypad term 
        $ withWindowHandler eventChan
        $ withSigIntHandler useSigINT eventChan
        $ f $ liftIO $ getEvent eventChan baseMap
  where
    wrapKeypad term g = finallyIO (liftIO (maybeOutput term keypadOn) >> g)
                        (maybeOutput term keypadOff)
    maybeOutput term cap = runTermOutput term $ 
            fromMaybe mempty (getCapability term cap)

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


getEvent :: TChan Event -> TreeMap Char Key -> IO Event
getEvent eventChan baseMap = allocaArray bufferSize loop
    where
        bufferSize = 100
        delay = 10000 -- 0.001 seconds
        -- TODO: instead of this loop, use hWaitForInput or hGetBuf and interrupt
        -- if an event lands in the eventChan first.
        -- But I'm not sure if those functions are interruptible.
        waitAndTryAgain buffer = threadDelay delay >> loop buffer
        loop :: Ptr Word8 -> IO Event
        loop buffer = do
          me <- atomically $ tryReadTChan eventChan
          case me of
            Just e -> return e
            Nothing -> do
              numRead <- hGetBufNonBlocking stdin buffer bufferSize
              if numRead < 1
                then waitAndTryAgain buffer
                else do
                  ws <- peekArray numRead buffer
                  let cs = map (toEnum . fromEnum) ws -- TODO: decode unicode here
                  let ks = map KeyInput $ lexKeys baseMap cs
                  case ks of
                    [] -> waitAndTryAgain buffer
                    k:ks' -> do 
                      atomically $ mapM_ (writeTChan eventChan) ks'
                      return k

tryReadTChan :: TChan a -> STM (Maybe a)
tryReadTChan chan = fmap Just (readTChan chan) `orElse` return Nothing
