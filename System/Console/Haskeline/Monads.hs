module System.Console.Haskeline.Monads(
                module Control.Monad.Trans,
                module System.Console.Haskeline.MonadException,
                ReaderT(..),
                runReaderT',
                asks,
                StateT(..),
                evalStateT',
                modify,
                update,
                MonadReader(..),
                MonadState(..)
                ) where

import Control.Monad.Trans
import System.Console.Haskeline.MonadException
import Data.IORef

import Control.Monad.Reader hiding (MonadReader,ask,asks)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State hiding (MonadState,get,put,modify)
import qualified Control.Monad.State as State

class Monad m => MonadReader r m where
    ask :: m r

instance Monad m => MonadReader r (ReaderT r m) where
    ask = Reader.ask

instance MonadReader r m => MonadReader r (ReaderT t m) where
    ask = lift ask
    
instance MonadReader r m => MonadReader r (StateT s m) where
    ask = lift ask

asks :: MonadReader r m => (r -> a) -> m a
asks f = liftM f ask

class Monad m => MonadState s m where
    get :: m s
    put :: s -> m ()

instance Monad m => MonadState s (StateT s m) where
    get = State.get
    put = State.put

instance MonadIO m => MonadState s (ReaderT (IORef s) m) where
    get = ask >>= liftIO . readIORef
    put x = ask >>= liftIO . flip writeIORef x

instance MonadIO m => MonadReader r (ReaderT (IORef r) m) where
    ask = get

instance MonadState s m => MonadState s (StateT t m) where
    get = lift get
    put = lift . put

instance MonadState s m => MonadState s (ReaderT r m) where
    get = lift get
    put = lift . put

modify :: MonadState s m => (s -> s) -> m ()
modify f = get >>= put . f

update :: MonadState s m => (s -> (a,s)) -> m a
update f = do
    s <- get
    let (x,s') = f s
    put s'
    return x

runReaderT' :: Monad m => r -> ReaderT r m a -> m a
runReaderT' = flip runReaderT

evalStateT' :: Monad m => s -> StateT s m a -> m a
evalStateT' = flip evalStateT
