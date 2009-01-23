module System.Console.Haskeline.Backend.Posix (
                        withPosixGetEvent,
                        posixLayouts,
                        tryGetLayouts,
                        PosixT,
                        runPosixT,
                        Encoders(),
                        posixEncode,
                        mapLines,
                        posixRunTerm
                 ) where

import Foreign
import Foreign.C.Types
import qualified Data.Map as Map
import System.Posix.Terminal hiding (Interrupt)
import Control.Monad
import Control.Concurrent hiding (throwTo)
import Control.Concurrent.Chan
import Data.Maybe (catMaybes)
import System.Posix.Signals.Exts
import System.Posix.IO(stdInput)
import Data.List
import System.IO
import qualified Data.ByteString as B
import Data.ByteString.Char8 as Char8 (pack)
import System.Environment

import System.Console.Haskeline.Monads
import System.Console.Haskeline.Key
import System.Console.Haskeline.Term
import System.Console.Haskeline.Prefs

import System.Console.Haskeline.Backend.IConv

import GHC.IOBase (haFD,FD)
import GHC.Handle (withHandle_)

#include <sys/ioctl.h>

-------------------
-- Window size

foreign import ccall ioctl :: FD -> CULong -> Ptr a -> IO CInt

posixLayouts :: Handle -> [IO (Maybe Layout)]
posixLayouts h = [ioctlLayout h, envLayout]

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
        => [(String,Key)] -> m (TreeMap Char Key)
getKeySequences tinfos = do
    sttys <- liftIO sttyKeys
    customKeySeqs <- getCustomKeySeqs
    -- note ++ acts as a union; so the below favors sttys over tinfos
    return $ listToTree
        $ ansiKeys ++ tinfos ++ sttys ++ customKeySeqs
  where
    getCustomKeySeqs = do
        kseqs <- asks customKeySequences
        termName <- liftIO $ handle (\(_::IOException) -> return "") (getEnv "TERM")
        let isThisTerm = maybe True (==termName)
        return $ map (\(_,cs,k) ->(cs,k))
            $ filter (\(kseqs',_,_) -> isThisTerm kseqs')
            $ kseqs


ansiKeys :: [(String, Key)]
ansiKeys = [("\ESC[D",  simpleKey LeftKey)
            ,("\ESC[C",  simpleKey RightKey)
            ,("\ESC[A",  simpleKey UpKey)
            ,("\ESC[B",  simpleKey DownKey)
            ,("\b",      simpleKey Backspace)]


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
    | k:ks <- lexKeys baseMap cs
            = metaKey k : ks
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
                        => Encoders -> [(String,Key)] -> (t m Event -> t m a) -> t m a
withPosixGetEvent enc termKeys f = do
    baseMap <- lift $ getKeySequences termKeys
    evenChan <- liftIO $ newChan
    withWindowHandler evenChan
        $ f $ liftIO $ getEvent enc baseMap evenChan

withWindowHandler :: MonadException m => Chan Event -> m a -> m a
withWindowHandler eventChan = withHandler windowChange $ 
    Catch $ writeChan eventChan WindowResize

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

getEvent :: Encoders -> TreeMap Char Key -> Chan Event -> IO Event
getEvent enc baseMap = keyEventLoop readKeyEvents
  where
    bufferSize = 32
    readKeyEvents = do
        -- Read at least one character of input, and more if available.
        -- In particular, the characters making up a control sequence will all
        -- be available at once, so we can process them together with lexKeys.
        threadWaitRead stdInput -- hWaitForInput doesn't work with -threaded on
                                -- ghc < 6.10 (#2363 in ghc's trac)
        bs <- B.hGetNonBlocking stdin bufferSize
        cs <- convert (localeToUnicode enc) bs
        return $ map KeyInput $ lexKeys baseMap cs

-- try to convert to the locale encoding using iconv.
-- if the buffer has an incomplete shift sequence,
-- read another byte of input and try again.
convert :: (B.ByteString -> IO (String,Result)) -> B.ByteString -> IO String
convert decoder bs = do
    (cs,result) <- decoder bs
    case result of
        Incomplete rest -> do
                    extra <- B.hGetNonBlocking stdin 1
                    if B.null extra
                        then return cs -- ignore the incomplete shift sequence
                                       -- since no more input is available.
                        else fmap (cs ++) $ convert decoder (rest `B.append` extra)
        _ -> return cs

-- NOTE: relys on getChar reading only 8 bytes.
getMultiByteChar :: (B.ByteString -> IO (String,Result)) -> IO Char
getMultiByteChar decoder = do
    b <- getChar
    cs <- convert decoder (Char8.pack [b])
    case cs of
        [] -> getMultiByteChar decoder
        (c:_) -> return c


-- fails if stdin is not a handle or if we couldn't access /dev/tty.
openTTY :: IO (Maybe Handle)
openTTY = do
    inIsTerm <- hIsTerminalDevice stdin
    if inIsTerm
        then handle (\(_::IOException) -> return Nothing) $ do
                h <- openFile "/dev/tty" WriteMode
                return (Just h)
        else return Nothing

posixRunTerm :: (Encoders -> Handle -> TermOps) -> IO RunTerm
posixRunTerm tOps = do
    fileRT <- fileRunTerm
    codeset <- getCodeset
    ttyH <- openTTY
    encoders <- liftM2 Encoders (openEncoder codeset) (openPartialDecoder codeset)
    case ttyH of
        Nothing -> return fileRT
        Just h -> return fileRT {
                    closeTerm = closeTerm fileRT >> hClose h,
                    -- NOTE: could also alloc Encoders once for each call to wrapRunTerm
                    termOps = Just (wrapRunTerm (wrapTerminalOps h) (tOps encoders h))
                }

type PosixT m = ReaderT Encoders (ReaderT Handle m)

data Encoders = Encoders {unicodeToLocale :: String -> IO B.ByteString,
                          localeToUnicode :: B.ByteString -> IO (String, Result)}

posixEncode :: (MonadIO m, MonadReader Encoders m) => String -> m B.ByteString
posixEncode str = do
    encoder <- asks unicodeToLocale
    liftIO $ encoder str

runPosixT :: Monad m => Encoders -> Handle -> PosixT m a -> m a
runPosixT enc h = runReaderT' h . runReaderT' enc

putTerm :: B.ByteString -> IO ()
putTerm str = B.putStr str >> hFlush stdout

fileRunTerm :: IO RunTerm
fileRunTerm = do
    oldLocale <- setLocale (Just "")
    codeset <- getCodeset
    let encoder str = join $ fmap ($ str) $ openEncoder codeset
    let decoder str = join $ fmap ($ str) $ openDecoder codeset
    decoder' <- openPartialDecoder codeset
    return RunTerm {putStrOut = \str -> encoder str >>= putTerm,
                closeTerm = setLocale oldLocale >> return (),
                wrapInterrupt = withSigIntHandler,
                encodeForTerm = encoder,
                decodeForTerm = decoder,
                getLocaleChar = getMultiByteChar decoder',
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

