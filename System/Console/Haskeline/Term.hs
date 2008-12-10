module System.Console.Haskeline.Term where

import System.Console.Haskeline.Monads
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Key
import System.Console.Haskeline.Prefs(Prefs)

import Control.Concurrent
import Control.Concurrent.STM
import Data.Typeable

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

data Event = WindowResize | KeyInput Key
                deriving Show

keyEventLoop :: IO [Event] -> TChan Event -> IO Event
keyEventLoop readEvents eventChan = do
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
            tid <- forkIO readerLoop
            (atomically $ readTChan eventChan)
                `finally` killThread tid
  where
    readerLoop = do
        es <- readEvents
        if null es
            then readerLoop
            else atomically $ mapM_ (writeTChan eventChan) es

tryReadTChan :: TChan a -> STM (Maybe a)
tryReadTChan chan = fmap Just (readTChan chan) `orElse` return Nothing

class (MonadReader Layout m, MonadIO m) => MonadLayout m where

data Interrupt = Interrupt
                deriving (Show,Typeable,Eq)

instance Exception Interrupt where

data Layout = Layout {width, height :: Int}
                    deriving (Show,Eq)

