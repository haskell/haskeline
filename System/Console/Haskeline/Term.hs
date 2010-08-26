module System.Console.Haskeline.Term where

import System.Console.Haskeline.Monads
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Key
import System.Console.Haskeline.Prefs(Prefs)
import System.Console.Haskeline.Completion(Completion)

import Control.Concurrent
import Data.Typeable
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Control.Exception.Extensible (fromException, AsyncException(..),bracket_)
import System.IO
import Control.Monad(liftM,when)
import System.IO.Error (isEOFError)

class (MonadReader Layout m, MonadException m) => Term m where
    reposition :: Layout -> LineChars -> m ()
    moveToNextLine :: LineChars -> m ()
    printLines :: [String] -> m ()
    drawLineDiff :: LineChars -> LineChars -> m ()
    clearLayout :: m ()
    ringBell :: Bool -> m ()

drawLine, clearLine :: Term m => LineChars -> m ()
drawLine = drawLineDiff ([],[])

clearLine = flip drawLineDiff ([],[])
    
data RunTerm = RunTerm {
            -- | Print unicode characters to the output (e.g., stdout or the terminal).
            putStrOut :: String -> IO (),
            encodeForTerm :: String -> IO ByteString,
            decodeForTerm :: ByteString -> IO String,
            termOps :: Either TermOps FileOps,
            wrapInterrupt :: MonadException m => m a -> m a,
            closeTerm :: IO ()
    }

-- | Operations needed for terminal-style interaction.
data TermOps = TermOps {
            getLayout :: IO Layout
            , withGetEvent :: (MonadException m, CommandMonad m)
                                => (m Event -> m a) -> m a
            , runTerm :: (MonadException m, CommandMonad m) => RunTermType m a -> m a
        }

-- | Operations needed for file-style interaction.
data FileOps = FileOps {
            getLocaleLine :: IO (Maybe String),
            getLocaleChar :: IO (Maybe Char),
            maybeReadNewline :: IO ()

        }

-- | Are we using terminal-style interaction?
isTerminalStyle :: RunTerm -> Bool
isTerminalStyle r = case termOps r of
                    Left TermOps{} -> True
                    _ -> False

-- Generic terminal actions which are independent of the Term being used.
-- Wrapped in a newtype so that we don't need RankNTypes.
newtype RunTermType m a = RunTermType (forall t . 
            (MonadTrans t, Term (t m), MonadException (t m), CommandMonad (t m))
                            => t m a)

class (MonadReader Prefs m , MonadReader Layout m)
        => CommandMonad m where
    runCompletion :: (String,String) -> m (String,[Completion])

instance (MonadTrans t, CommandMonad m, MonadReader Prefs (t m),
        MonadReader Layout (t m))
            => CommandMonad (t m) where
    runCompletion = lift . runCompletion

-- Utility function for drawLineDiff instances.
matchInit :: Eq a => [a] -> [a] -> ([a],[a])
matchInit (x:xs) (y:ys)  | x == y = matchInit xs ys
matchInit xs ys = (xs,ys)

data Event = WindowResize | KeyInput [Key] | ErrorEvent SomeException
                deriving Show

keyEventLoop :: IO [Event] -> Chan Event -> IO Event
keyEventLoop readEvents eventChan = do
    -- first, see if any events are already queued up (from a key/ctrl-c
    -- event or from a previous call to getEvent where we read in multiple
    -- keys)
    isEmpty <- isEmptyChan eventChan
    if not isEmpty
        then readChan eventChan
        else do
            lock <- newEmptyMVar
            tid <- forkIO $ handleErrorEvent (readerLoop lock)
            readChan eventChan `finally` do
                            putMVar lock ()
                            killThread tid
  where
    readerLoop lock = do
        es <- readEvents
        if null es
            then readerLoop lock
            else -- Use the lock to work around the fact that writeList2Chan
                 -- isn't atomic.  Otherwise, some events could be ignored if
                 -- the subthread is killed before it saves them in the chan.
                 bracket_ (putMVar lock ()) (takeMVar lock) $ 
                    writeList2Chan eventChan es
    handleErrorEvent = handle $ \e -> case fromException e of
                                Just ThreadKilled -> return ()
                                _ -> writeChan eventChan (ErrorEvent e)


data Interrupt = Interrupt
                deriving (Show,Typeable,Eq)

instance Exception Interrupt where

data Layout = Layout {width, height :: Int}
                    deriving (Show,Eq)

-----------------------------------
-- Utility functions for the various backends.

-- | Utility function since we're not using the new IO library yet.
hWithBinaryMode :: MonadException m => Handle -> m a -> m a
#if __GLASGOW_HASKELL__ >= 611
hWithBinaryMode h = bracket (liftIO $ hGetEncoding h)
                        (maybe (return ()) (liftIO . hSetEncoding h))
                        . const . (liftIO (hSetBinaryMode h True) >>)
#else
hWithBinaryMode _ = id
#endif


-- | Utility function to correctly get a ByteString line of input.
hGetLine :: Handle -> IO (Maybe ByteString)
hGetLine h = do
    atEOF <- hIsEOF h
    if atEOF then return Nothing else fmap Just $ do
    -- It's more efficient to use B.getLine, but that function throws an
    -- error if the Handle (e.g., stdin) is set to NoBuffering.
    buff <- hGetBuffering h
    if buff == NoBuffering
        then hWithBinaryMode h $ fmap B.pack $ System.IO.hGetLine h
        else B.hGetLine h

-- If another character is immediately available, and it is a newline, consume it.
--
-- Two portability fixes:
-- 
-- 1) Note that in ghc-6.8.3 and earlier, hReady returns False at an EOF,
-- whereas in ghc-6.10.1 and later it throws an exception.  (GHC trac #1063).
-- This code handles both of those cases.
--
-- 2) Also note that on Windows with ghc<6.10, hReady may not behave correctly (#1198)
-- The net result is that this might cause
-- But this function will generally only be used when reading buffered input
-- (since stdin isn't a terminal), so it should probably be OK.
hMaybeReadNewline :: Handle -> IO ()
hMaybeReadNewline h = returnOnEOF () $ do
    ready <- hReady h
    when ready $ do
        c <- hLookAhead h
        when (c == '\n') $ getChar >> return ()

returnOnEOF :: MonadException m => a -> m a -> m a
returnOnEOF x = handle $ \e -> if isEOFError e
                                then return x
                                else throwIO e

-- | A simple, MaybeT-like combinator.
maybeThen :: Monad m => m (Maybe a) -> (a -> m b) -> m (Maybe b)
maybeThen f g = f >>= maybe (return Nothing) (liftM Just . g)
