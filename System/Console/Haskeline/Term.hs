module System.Console.Haskeline.Term where

import System.Console.Haskeline.Monads
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Key
import System.Console.Haskeline.Prefs(Prefs)

import Control.Concurrent
import Control.Concurrent.Chan
import Data.Typeable
import Data.ByteString (ByteString)
import Control.Exception.Extensible (fromException, AsyncException(..),bracket_)

class (MonadReader Layout m, MonadException m) => Term m where
    reposition :: Layout -> LineChars -> m ()
    moveToNextLine :: LineChars -> m ()
    printLines :: [String] -> m ()
    drawLineDiff :: LineChars -> LineChars -> m ()
    clearLayout :: m ()
    ringBell :: Bool -> m ()
    
-- putStrOut is the right way to send unicode chars to stdout.
-- termOps being Nothing means we should read the input as a UTF-8 file.
data RunTerm = RunTerm {
            putStrOut :: String -> IO (),
            encodeForTerm :: String -> IO ByteString,
            decodeForTerm :: ByteString -> IO String,
            getLocaleChar :: IO Char,
            termOps :: Maybe TermOps,
            wrapInterrupt :: MonadException m => m a -> m a,
            closeTerm :: IO ()
    }

data TermOps = TermOps {runTerm :: RunTermType,
                        getLayout :: IO Layout}

type RunTermType = forall m a . (MonadLayout m, MonadException m, MonadReader Prefs m) 
                    => (forall t . (MonadTrans t, Term (t m), MonadException (t m)) 
                            => (t m Event -> t m a)) -> m a


-- Utility function for drawLineDiff instances.
matchInit :: Eq a => [a] -> [a] -> ([a],[a])
matchInit (x:xs) (y:ys)  | x == y = matchInit xs ys
matchInit xs ys = (xs,ys)

data Event = WindowResize | KeyInput Key | ErrorEvent SomeException
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

class (MonadReader Layout m, MonadIO m) => MonadLayout m where

data Interrupt = Interrupt
                deriving (Show,Typeable,Eq)

instance Exception Interrupt where

data Layout = Layout {width, height :: Int}
                    deriving (Show,Eq)

