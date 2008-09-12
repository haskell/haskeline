module System.Console.Haskeline.Term where

import System.Console.Haskeline.Monads
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Command

import Control.Concurrent
import Control.Concurrent.STM

class MonadIO m => Term m where
    withReposition :: Layout -> m a -> m a
    moveToNextLine :: LineState s => s -> m ()
    printLines :: [String] -> m ()
    drawLineDiff :: (LineState s, LineState r)
                    => String -> s -> r -> m ()
    clearLayout :: m ()
    ringBell :: Bool -> m ()
    

data RunTerm = RunTerm {
            getLayout :: IO Layout,
            withGetEvent :: forall m a . MonadException m => Bool -> (m Event -> m a) -> m a,
            runTerm :: RunTermType,
            putStrTerm :: String -> IO ()
    }

type RunTermType = forall m a . (MonadLayout m, MonadException m) 
                    => (forall t . (MonadTrans t, Term (t m), MonadException (t m)) => t m a) 
                        -> m a

-- Utility function for drawLineDiff instances.
matchInit :: Eq a => [a] -> [a] -> ([a],[a])
matchInit (x:xs) (y:ys)  | x == y = matchInit xs ys
matchInit xs ys = (xs,ys)

keyEventLoop :: (TChan Event -> IO ()) -> TChan Event -> IO Event
keyEventLoop readKey eventChan = do
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
            tid <- forkIO (readKey eventChan)
            e <- atomically $ readTChan eventChan -- key or other event
            killThread tid
            return e

tryReadTChan :: TChan a -> STM (Maybe a)
tryReadTChan chan = fmap Just (readTChan chan) `orElse` return Nothing

class (MonadReader Layout m, MonadIO m) => MonadLayout m where
