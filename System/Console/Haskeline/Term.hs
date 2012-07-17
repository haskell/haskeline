module System.Console.Haskeline.Term where

import System.Console.Haskeline.Monads
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Key
import System.Console.Haskeline.Prefs(Prefs)
import System.Console.Haskeline.Completion(Completion)

import Control.Concurrent
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Word
import Control.Exception (fromException, AsyncException(..),bracket_)
import Data.Typeable
import System.IO
import Control.Monad(liftM,when,guard)
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
            -- | Write unicode characters to stdout.
            putStrOut :: String -> IO (),
            termOps :: Either TermOps FileOps,
            wrapInterrupt :: forall a . IO a -> IO a,            
            closeTerm :: IO ()
    }

-- | Operations needed for terminal-style interaction.
data TermOps = TermOps {
            getLayout :: IO Layout
            , withGetEvent :: CommandMonad m => (m Event -> m a) -> m a
            , evalTerm :: forall m . CommandMonad m => EvalTerm m
            , saveUnusedKeys :: [Key] -> IO ()
        }

-- | Operations needed for file-style interaction.
data FileOps = FileOps {
            inputHandle :: Handle, -- ^ e.g. for turning off echoing.
            getLocaleLine :: MaybeT IO String,
            getLocaleChar :: MaybeT IO Char,
            maybeReadNewline :: IO ()
        }

-- | Are we using terminal-style interaction?
isTerminalStyle :: RunTerm -> Bool
isTerminalStyle r = case termOps r of
                    Left TermOps{} -> True
                    _ -> False

-- Specific, hidden terminal action type
-- Generic terminal actions which are independent of the Term being used.
data EvalTerm m
    = forall n . (Term n, CommandMonad n)
            => EvalTerm (forall a . n a -> m a) (forall a . m a -> n a)

mapEvalTerm :: (forall a . n a -> m a) -> (forall a . m a -> n a)
        -> EvalTerm n -> EvalTerm m
mapEvalTerm eval liftE (EvalTerm eval' liftE')
    = EvalTerm (eval . eval') (liftE' . liftE)

data Interrupt = Interrupt
                deriving (Show,Typeable,Eq)

instance Exception Interrupt where



class (MonadReader Prefs m , MonadReader Layout m, MonadException m)
        => CommandMonad m where
    runCompletion :: (String,String) -> m (String,[Completion])

instance (MonadTrans t, CommandMonad m, MonadReader Prefs (t m),
        MonadException (t m),
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

saveKeys :: Chan Event -> [Key] -> IO ()
saveKeys ch = writeChan ch . KeyInput

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

-- | Utility function for changing a property of a terminal for the duration of
-- a computation.
bracketSet :: (Eq a, MonadException m) => IO a -> (a -> IO ()) -> a -> m b -> m b
bracketSet getState set newState f = bracket (liftIO getState)
                            (liftIO . set)
                            (\_ -> liftIO (set newState) >> f)


-- | Returns one 8-bit word.  Needs to be wrapped by hWithBinaryMode.
hGetByte :: Handle -> MaybeT IO Word8
hGetByte h = do
    eof <- liftIO $ hIsEOF h
    guard (not eof)
    liftIO $ liftM (toEnum . fromEnum) $ hGetChar h


-- | Utility function to correctly get a ByteString line of input.
hGetLine :: Handle -> MaybeT IO ByteString
hGetLine h = do
    atEOF <- liftIO $ hIsEOF h
    guard (not atEOF)
    -- It's more efficient to use B.getLine, but that function throws an
    -- error if the Handle (e.g., stdin) is set to NoBuffering.
    buff <- liftIO $ hGetBuffering h
    liftIO $ if buff == NoBuffering
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
