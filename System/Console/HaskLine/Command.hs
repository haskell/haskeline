module System.Console.HaskLine.Command where

import System.Console.HaskLine.LineState
import System.Console.Terminfo
import Data.Maybe

import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import System.Console.Terminfo
import System.Posix (Fd,stdOutput)
import System.Posix.Terminal
import Control.Monad
import Control.Monad.Trans

import Data.Bits

data Key = KeyChar Char | KeySpecial SKey
                deriving (Show,Eq,Ord)
data SKey = KeyLeft | KeyRight | KeyUp | KeyDown
            | Backspace | DeleteForward | KillLine
                deriving (Eq,Ord,Enum,Show)

-- Easy translation of control characters; e.g., Ctrl-G or Ctrl-g or ^G
controlKey :: Char -> Key
controlKey '?' = KeyChar (toEnum 127)
controlKey c = KeyChar $ toEnum $ fromEnum c .&. complement (bit 5 .|. bit 6)

getKeySequences :: Terminal -> IO (Map.Map String SKey)
getKeySequences term = do
    sttys <- sttyKeys
    return $ Map.union sttys (terminfoKeys term)

terminfoKeys :: Terminal -> Map.Map String SKey
terminfoKeys term = Map.fromList $ catMaybes 
                    $ map getSequence keyCapabilities
        where getSequence (cap,x) = getCapability term $ do 
                            keys <- cap
                            return (keys,x)

keyCapabilities = [(keyLeft,KeyLeft),
                (keyRight,KeyRight),
                (keyUp,KeyUp),
                (keyDown,KeyDown),
                (keyBackspace,Backspace),
                (keyDeleteChar,DeleteForward)]

sttyKeys :: IO (Map.Map String SKey)
sttyKeys = do
    attrs <- getTerminalAttributes stdOutput
    let getStty (k,c) = do {str <- controlChar attrs k; return ([str],c)}
    return $ Map.fromList $ catMaybes
            $ map getStty [(Erase,Backspace),(Kill,KillLine)]

getKey :: [(String, SKey)] -> IO Key
getKey ms = do 
    c <- getChar
    case mapMaybe (matchHead c) ms of
        [] -> return (KeyChar c)
        [("",k)] -> return (KeySpecial k)
        ms' -> getKey ms'
  where
    matchHead c (d:ds,k)  | c == d = Just (ds,k)
    matchHead _ _                  = Nothing


        
{-- todo: some commands only change the linestate, don't require
 a full refresh:
 data Command m = Change (LSCHANGE) | Refresh (Linestate -> m LineState)
 --}

data Command m = Finish | Command (LineState -> m Result)
                | RedrawLine {shouldClearScreen :: Bool}

data Result = Changed LineState | PrintLines (Layout -> [String]) LineState
                                   


isFinish :: Command m -> Bool
isFinish Finish = True
isFinish _ = False

type Commands m = Map.Map Key (Command m)

simpleCommands :: Monad m => Commands m
simpleCommands = Map.fromList $ [
                    (KeyChar '\n', Finish)
                    ,(KeySpecial KeyLeft, pureCommand goLeft)
                    ,(KeySpecial KeyRight, pureCommand goRight)
                    ,(KeySpecial Backspace, pureCommand deletePrev)
                    ,(KeySpecial DeleteForward, pureCommand deleteNext)
                    -- ,(KeySpecial KillLine, pureCommand killLine)
                    ] ++ map insertionCommand [' '..'~']
            
pureCommand :: Monad m => LineChange -> Command m
pureCommand f = Command (return . Changed . f)

insertionCommand :: Monad m => Char -> (Key,Command m)
insertionCommand c = (KeyChar c, pureCommand $ insertChar c)

changeCommand :: Monad m => (LineState -> m LineState) -> Command m
changeCommand f = Command $ \ls -> liftM Changed (f ls)
                    
newtype CommandT s m a = CommandT {runCommandT :: s -> m (a,s)}

evalCommandT :: Monad m => s -> CommandT s m a -> m a
evalCommandT s f = liftM fst $ runCommandT f s

instance Monad m => Monad (CommandT s m) where
    return x = CommandT $ \s -> return (x,s)
    CommandT f >>= g = CommandT $ \s -> do
                                (x,s') <- f s
                                runCommandT (g x) s'

instance MonadTrans (CommandT s) where
    lift f = CommandT $ \s -> do {x <- f; return (x,s)}

instance MonadIO m => MonadIO (CommandT s m) where
    liftIO f = CommandT $ \s -> do {x <- liftIO f; return (x,s)}

class MonadIO m => MonadIO1 m where
    liftIO1 :: (forall a . IO a -> IO a) -> m a -> m a

instance MonadIO1 IO where
    liftIO1 = id

instance MonadIO1 m => MonadIO1 (CommandT s m) where
    liftIO1 f (CommandT g) = CommandT $ liftIO1 f . g
    
class Monad m => MonadCmd s m where
    getState :: m s
    putState :: s -> m ()

instance Monad m => MonadCmd s (CommandT s m) where
    getState = CommandT $ \s -> return (s,s)
    putState s = CommandT $ \_ -> return ((),s)

instance MonadCmd s m => MonadCmd s (CommandT t m) where
    getState = lift getState
    putState s = lift (putState s)

-- A useful combinator
updateState :: MonadCmd s m => (s -> (a,s)) -> m a
updateState f = do
    s <- getState
    let (x,s') = f s
    putState s'
    return x

modifyState :: MonadCmd s m => (s -> s) -> m ()
modifyState f = getState >>= putState . f

