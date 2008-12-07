module System.Console.Haskeline.Backend.Posix (
                        withPosixGetEvent,
                        getPosixLayout,
                        mapLines,
                        putTerm,
                        posixRunTerm
                 ) where

import Foreign
import Foreign.C.Types
import qualified Data.Map as Map
import System.Console.Terminfo
import System.Posix.Terminal hiding (Interrupt)
import Control.Monad
import Control.Concurrent hiding (throwTo)
import Control.Concurrent.STM
import Data.Maybe
import System.Posix.Signals.Exts
import System.Posix.IO(stdInput)
import Data.List
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import System.Environment

import System.Console.Haskeline.Monads
import System.Console.Haskeline.Key
import System.Console.Haskeline.Term
import System.Console.Haskeline.Prefs

import GHC.IOBase (haFD,FD)
import GHC.Handle (withHandle_)

#include <sys/ioctl.h>

-------------------
-- Window size

foreign import ccall ioctl :: CInt -> CULong -> Ptr a -> IO CInt

getPosixLayout :: Handle -> Maybe Terminal -> IO Layout
getPosixLayout h term = tryGetLayouts [ioctlLayout h, envLayout, tinfoLayout term]

ioctlLayout :: Handle -> IO (Maybe Layout)
ioctlLayout h = allocaBytes (#size struct winsize) $ \ws -> do
                fd <- unsafeHandleToFD h
                ret <- ioctl fd (#const TIOCGWINSZ) ws
                rows :: CUShort <- (#peek struct winsize,ws_row) ws
                cols :: CUShort <- (#peek struct winsize,ws_col) ws
                if ret >= 0
                    then return $ Just Layout {height=fromEnum rows,width=fromEnum cols}
                    else return Nothing

unsafeHandleToFD :: Handle -> IO FD
unsafeHandleToFD h = withHandle_ "unsafeHandleToFd" h (return . haFD)

envLayout :: IO (Maybe Layout)
envLayout = handle (\(_::IOException) -> return Nothing) $ do
    -- note the handle catches both undefined envs and bad reads
    r <- getEnv "ROWS"
    c <- getEnv "COLUMNS"
    return $ Just $ Layout {height=read r,width=read c}

tinfoLayout :: Maybe Terminal -> IO (Maybe Layout)
tinfoLayout = maybe (return Nothing) $ \t -> return $ getCapability t $ do
                        r <- termColumns
                        c <- termLines
                        return Layout {height=r,width=c}

tryGetLayouts :: [IO (Maybe Layout)] -> IO Layout
tryGetLayouts [] = return Layout {height=24,width=80}
tryGetLayouts (f:fs) = do
    ml <- f
    case ml of
        Just l | height l > 2 && width l > 2 -> return l
        _ -> tryGetLayouts fs


--------------------
-- Key sequences

getKeySequences :: (MonadIO m, MonadReader Prefs m)
        => Maybe Terminal -> m (TreeMap Char Key)
getKeySequences term = do
    sttys <- liftIO sttyKeys
    customKeySeqs <- asks customKeySequences
    let tinfos = maybe [] terminfoKeys term
    -- note ++ acts as a union; so the below favors sttys over tinfos
    return $ listToTree
        $ ansiKeys ++ tinfos ++ sttys ++ customKeySeqs


ansiKeys :: [(String, Key)]
ansiKeys = [("\ESC[D",  simpleKey LeftKey)
            ,("\ESC[C",  simpleKey RightKey)
            ,("\ESC[A",  simpleKey UpKey)
            ,("\ESC[B",  simpleKey DownKey)
            ,("\b",      simpleKey Backspace)]

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
                ]

sttyKeys :: IO [(String, Key)]
sttyKeys = do
    attrs <- getTerminalAttributes stdInput
    let getStty (k,c) = do {str <- controlChar attrs k; return ([str],c)}
    return $ catMaybes $ map getStty [(Erase,simpleKey Backspace),(Kill,simpleKey KillLine)]
                        
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
lexKeys baseMap ('\ESC':cs)
-- TODO: what's the right thing ' to do here?
    | Key _ k:ks <- lexKeys baseMap cs
            = Key (Just Meta) k : ks
lexKeys baseMap (c:cs) = simpleChar c : lexKeys baseMap cs

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

withPosixGetEvent :: (MonadTrans t, MonadIO m, MonadException (t m), MonadReader Prefs m) 
                        => Handle -> Maybe Terminal -> (t m Event -> t m a) -> t m a
withPosixGetEvent h term f = do
    baseMap <- lift $ getKeySequences term
    eventChan <- liftIO $ newTChanIO
    wrapKeypad h term $ withWindowHandler eventChan
        $ f $ liftIO $ getEvent baseMap eventChan

-- If the keypad on/off capabilities are defined, wrap the computation with them.
wrapKeypad :: MonadException m => Handle -> Maybe Terminal -> m a -> m a
wrapKeypad h = maybe id $ \term f -> (maybeOutput term keypadOn >> f) 
                            `finally` maybeOutput term keypadOff
  where
    maybeOutput term cap = liftIO $ hRunTermOutput h term $
                            fromMaybe mempty (getCapability term cap)

withWindowHandler :: MonadException m => TChan Event -> m a -> m a
withWindowHandler eventChan = withHandler windowChange $ 
    Catch $ atomically $ writeTChan eventChan WindowResize

withSigIntHandler :: MonadException m => m a -> m a
withSigIntHandler f = do
    tid <- liftIO myThreadId 
    withHandler keyboardSignal 
            (Catch (throwTo tid Interrupt))
            f

withHandler :: MonadException m => Signal -> Handler -> m a -> m a
withHandler signal handler f = do
    old_handler <- liftIO $ installHandler signal handler Nothing
    f `finally` liftIO (installHandler signal old_handler Nothing)

getEvent :: TreeMap Char Key -> TChan Event -> IO Event
getEvent baseMap = keyEventLoop readKeyEvents
  where
    bufferSize = 100
    readKeyEvents eventChan = do
        -- Read at least one character of input, and more if available.
        -- In particular, the characters making up a control sequence will all
        -- be available at once, so we can process them together with lexKeys.
        threadWaitRead stdInput
        bs <- B.hGetNonBlocking stdin bufferSize
        let cs = UTF8.toString bs
        let ks = map KeyInput $ lexKeys baseMap cs
        if null ks
            then readKeyEvents eventChan
            else atomically $ mapM_ (writeTChan eventChan) ks

-- fails if stdin is not a handle or if we couldn't access /dev/tty.
openTTY :: IO (Maybe Handle)
openTTY = do
    inIsTerm <- hIsTerminalDevice stdin
    if inIsTerm
        then handle (\(_::IOException) -> return Nothing) $ do
                h <- openFile "/dev/tty" WriteMode
                return (Just h)
        else return Nothing

posixRunTerm :: (Handle -> TermOps) -> IO RunTerm
posixRunTerm tOps = do
    ttyH <- openTTY
    case ttyH of
        Nothing -> return fileRunTerm
        Just h -> return RunTerm {
                    putStrOut = putTerm stdout,
                    closeTerm = hClose h,
                    wrapInterrupt = withSigIntHandler,
                    termOps = Just (wrapRunTerm (wrapTerminalOps h) (tOps h))
                }

putTerm :: Handle -> String -> IO ()
putTerm h str = B.hPutStr h (UTF8.fromString str) >> hFlush h

fileRunTerm :: RunTerm
fileRunTerm = RunTerm {putStrOut = putTerm stdout,
                closeTerm = return (),
                wrapInterrupt = withSigIntHandler,
                termOps = Nothing
                }


-- NOTE: If we set stdout to NoBuffering, there can be a flicker effect when many
-- characters are printed at once.  We'll keep it buffered here, and let the Draw
-- monad manually flush outputs that don't print a newline.
wrapTerminalOps:: MonadException m => Handle -> m a -> m a
wrapTerminalOps outH =
    bracketSet (hGetBuffering stdin) (hSetBuffering stdin) NoBuffering
    . bracketSet (hGetBuffering outH) (hSetBuffering outH) LineBuffering
    . bracketSet (hGetEcho stdin) (hSetEcho stdin) False

wrapRunTerm :: (forall m a . MonadException m => m a -> m a) -> TermOps -> TermOps
wrapRunTerm wrap tops = tops {runTerm = \getE -> wrap (runTerm tops getE)
                                }

bracketSet :: (Eq a, MonadException m) => IO a -> (a -> IO ()) -> a -> m b -> m b
bracketSet getState set newState f = do
    oldState <- liftIO getState
    if oldState == newState
        then f
        else finally (liftIO (set newState) >> f) (liftIO (set oldState))

