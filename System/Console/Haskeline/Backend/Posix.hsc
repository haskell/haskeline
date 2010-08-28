module System.Console.Haskeline.Backend.Posix (
                        withPosixGetEvent,
                        posixLayouts,
                        tryGetLayouts,
                        PosixT,
                        runPosixT,
                        Handles(..),
                        Encoders(),
                        posixEncode,
                        mapLines,
                        posixRunTerm,
                        fileRunTerm
                 ) where

import Foreign
import Foreign.C.Types
import qualified Data.Map as Map
import System.Posix.Terminal hiding (Interrupt)
import Control.Monad
import Control.Concurrent hiding (throwTo)
import Data.Maybe (catMaybes)
import System.Posix.Signals.Exts
import System.Posix.Types(Fd(..))
import Data.List
import System.IO
import qualified Data.ByteString as B
import System.Environment

import System.Console.Haskeline.Monads
import System.Console.Haskeline.Key
import System.Console.Haskeline.Term as Term
import System.Console.Haskeline.Prefs

import System.Console.Haskeline.Backend.IConv

#if __GLASGOW_HASKELL__ >= 611
import GHC.IO.FD (fdFD)
import Data.Dynamic (cast)
import System.IO.Error
import GHC.IO.Exception
import GHC.IO.Handle.Types hiding (getState)
import GHC.IO.Handle.Internals
import System.Posix.Internals (FD)
#else
import GHC.IOBase(haFD,FD)
import GHC.Handle (withHandle_)
#endif

#ifdef USE_TERMIOS_H
#include <termios.h>
#endif
#include <sys/ioctl.h>

-----------------------------------------------
-- Input/output handles
data Handles = Handles {hIn, hOut :: Handle}

-------------------
-- Window size

foreign import ccall ioctl :: FD -> CULong -> Ptr a -> IO CInt

posixLayouts :: Handles -> [IO (Maybe Layout)]
posixLayouts h = [ioctlLayout $ hOut h, envLayout]

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
#if __GLASGOW_HASKELL__ >= 611
unsafeHandleToFD h =
  withHandle_ "unsafeHandleToFd" h $ \Handle__{haDevice=dev} -> do
  case cast dev of
    Nothing -> ioError (ioeSetErrorString (mkIOError IllegalOperation
                                           "unsafeHandleToFd" (Just h) Nothing)
                        "handle is not a file descriptor")
    Just fd -> return (fdFD fd)
#else
unsafeHandleToFD h = withHandle_ "unsafeHandleToFd" h (return . haFD)
#endif

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
        => Handles -> [(String,Key)] -> m (TreeMap Char Key)
getKeySequences h tinfos = do
    sttys <- liftIO $ sttyKeys h
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
            ,("\b",      simpleKey Backspace)
            -- ctrl-left/right aren't a standard
            -- part of terminfo, but enough people have complained
            -- that I've decided to hard-code them in.
            -- (Note they will be overridden by terminfo or .haskeline.)
            -- These appear to be the most common bindings:
            -- xterm:
            ,("\ESC[1;5D", ctrlKey $ simpleKey LeftKey)
            ,("\ESC[1;5C", ctrlKey $ simpleKey RightKey)
            -- Terminal.app:
            ,("\ESC[5D", ctrlKey $ simpleKey LeftKey)
            ,("\ESC[5C", ctrlKey $ simpleKey RightKey)
            -- rxvt: (Note: these will be superceded by e.g. xterm-color,
            -- which uses them as regular arrow keys.)
            ,("\ESC[OD", ctrlKey $ simpleKey LeftKey)
            ,("\ESC[OC", ctrlKey $ simpleKey RightKey)
            ]


sttyKeys :: Handles -> IO [(String, Key)]
sttyKeys h = do
    fd <- unsafeHandleToFD $ hIn h
    attrs <- getTerminalAttributes (Fd fd)
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

withPosixGetEvent :: (MonadException m, MonadReader Prefs m) 
        => Chan Event -> Handles -> Encoders -> [(String,Key)]
                -> (m Event -> m a) -> m a
withPosixGetEvent eventChan h enc termKeys f = wrapTerminalOps h $ do
    baseMap <- getKeySequences h termKeys
    withWindowHandler eventChan
        $ f $ liftIO $ getEvent h enc baseMap eventChan

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

getEvent :: Handles -> Encoders -> TreeMap Char Key -> Chan Event -> IO Event
getEvent Handles {hIn=h} enc baseMap = keyEventLoop readKeyEvents
  where
    bufferSize = 32
    readKeyEvents = do
        -- Read at least one character of input, and more if available.
        -- In particular, the characters making up a control sequence will all
        -- be available at once, so we can process them together with lexKeys.
        blockUntilInput h
        bs <- B.hGetNonBlocking h bufferSize
        cs <- convert h (localeToUnicode enc) bs
        return [KeyInput $ lexKeys baseMap cs]

-- Different versions of ghc work better using different functions.
blockUntilInput :: Handle -> IO ()
#if __GLASGOW_HASKELL__ >= 611
-- threadWaitRead doesn't work with the new ghc IO library,
-- because it keeps a buffer even when NoBuffering is set.
blockUntilInput h = hWaitForInput h (-1) >> return ()
#else
-- hWaitForInput doesn't work with -threaded on ghc < 6.10
-- (#2363 in ghc's trac)
blockUntilInput h = unsafeHandleToFd h >>= threadWaitRead
#endif

-- try to convert to the locale encoding using iconv.
-- if the buffer has an incomplete shift sequence,
-- read another byte of input and try again.
convert :: Handle -> (B.ByteString -> IO (String,Result))
            -> B.ByteString -> IO String
convert h decoder bs = do
    (cs,result) <- decoder bs
    case result of
        Incomplete rest -> do
                    extra <- B.hGetNonBlocking h 1
                    if B.null extra
                        then return (cs ++ "?")
                        else fmap (cs ++)
                                $ convert h decoder (rest `B.append` extra)
        Invalid rest -> fmap ((cs ++) . ('?':)) $ convert h decoder (B.drop 1 rest)
        _ -> return cs

getMultiByteChar :: Handle -> (B.ByteString -> IO (String,Result))
                        -> MaybeT IO Char
getMultiByteChar h decoder = hWithBinaryMode h $ do
    b <- hGetByte h
    cs <- liftIO $ convert h decoder (B.pack [b])
    case cs of
        [] -> return '?' -- shouldn't happen, but doesn't hurt to be careful.
        (c:_) -> return c


-- fails if stdin is not a handle or if we couldn't access /dev/tty.
openTTY :: MaybeT IO Handle
openTTY = MaybeT $ do
    inIsTerm <- hIsTerminalDevice stdin
    if inIsTerm
        then handle (\(_::IOException) -> return Nothing) $ do
            -- NB: we open the tty as a binary file since otherwise the terminfo
            -- backend, which writes output as Chars, would double-encode on ghc-6.12.
                h <- openBinaryFile "/dev/tty" WriteMode
                return (Just h)
        else return Nothing

posixRunTerm :: (Encoders -> Handles -> TermOps) -> MaybeT IO RunTerm
posixRunTerm tOps = openTTY >>= \h_out -> liftIO $ do
    let h_in = stdin
    codeset <- getCodeset
    encoders <- liftM2 Encoders (openEncoder codeset) (openPartialDecoder codeset)
    fileRT <- fileRunTerm h_in
    return fileRT {
                closeTerm = closeTerm fileRT >> hClose h_out,
                -- NOTE: could also alloc Encoders once for each call to wrapRunTerm
                termOps = Left $ tOps encoders Handles { hIn = h_in, hOut = h_out }
            }

type PosixT m = ReaderT Encoders (ReaderT Handles m)

data Encoders = Encoders {unicodeToLocale :: String -> IO B.ByteString,
                          localeToUnicode :: B.ByteString -> IO (String, Result)}

posixEncode :: (MonadIO m, MonadReader Encoders m) => String -> m B.ByteString
posixEncode str = do
    encoder <- asks unicodeToLocale
    liftIO $ encoder str

runPosixT :: Monad m => Encoders -> Handles -> PosixT m a -> m a
runPosixT enc h = runReaderT' h . runReaderT' enc

putTerm :: Handle -> B.ByteString -> IO ()
putTerm h str = B.hPutStr h str >> hFlush h

fileRunTerm :: Handle -> IO RunTerm
fileRunTerm h_in = do
    let h_out = stdout
    oldLocale <- setLocale (Just "")
    codeset <- getCodeset
    let encoder str = join $ fmap ($ str) $ openEncoder codeset
    let decoder str = join $ fmap ($ str) $ openDecoder codeset
    decoder' <- openPartialDecoder codeset
    return RunTerm {putStrOut = encoder >=> putTerm h_out,
                closeTerm = setLocale oldLocale >> return (),
                wrapInterrupt = withSigIntHandler,
                encodeForTerm = encoder,
                decodeForTerm = decoder,
                termOps = Right FileOps {
                            getLocaleChar = getMultiByteChar h_in decoder',
                            maybeReadNewline = hMaybeReadNewline h_in,
                            getLocaleLine = Term.hGetLine h_in
                                                >>= liftIO . decoder
                        }

                }

-- NOTE: If we set stdout to NoBuffering, there can be a flicker effect when many
-- characters are printed at once.  We'll keep it buffered here, and let the Draw
-- monad manually flush outputs that don't print a newline.
wrapTerminalOps :: MonadException m => Handles -> m a -> m a
wrapTerminalOps Handles {hIn = h_in, hOut = h_out} = 
    bracketSet (hGetBuffering h_in) (hSetBuffering h_in) NoBuffering
    -- TODO: block buffering?  Certain \r and \n's are causing flicker...
    -- - moving to the right
    -- - breaking line after offset widechar?
    . bracketSet (hGetBuffering h_out) (hSetBuffering h_out) LineBuffering
    . bracketSet (hGetEcho h_in) (hSetEcho h_in) False
    . hWithBinaryMode h_in

bracketSet :: (Eq a, MonadException m) => IO a -> (a -> IO ()) -> a -> m b -> m b
bracketSet getState set newState f = bracket (liftIO getState)
                            (liftIO . set)
                            (\_ -> liftIO (set newState) >> f)
