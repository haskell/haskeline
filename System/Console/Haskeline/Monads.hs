module System.Console.Haskeline.Monads(
                module Control.Monad.Trans,
                module System.Console.Haskeline.MonadException,
                ReaderT(..),
                runReaderT',
                asks,
                StateT(..),
                evalStateT',
                gets,
                modify,
                update,
                MonadReader(..),
                MonadState(..)
                ) where

import Control.Monad.Trans
import System.Console.Haskeline.MonadException

import Control.Monad.Reader hiding (MonadReader,ask,asks,local)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State hiding (MonadState,get,put,gets,modify)
import qualified Control.Monad.State as State

class Monad m => MonadReader r m where
    ask :: m r

instance Monad m => MonadReader r (ReaderT r m) where
    ask = Reader.ask

instance Monad m => MonadReader s (StateT s m) where
    ask = get

instance (MonadReader r m, MonadTrans t, Monad (t m)) => MonadReader r (t m) where
    ask = lift ask

asks :: MonadReader r m => (r -> a) -> m a
asks f = liftM f ask

class Monad m => MonadState s m where
    get :: m s
    put :: s -> m ()

instance Monad m => MonadState s (StateT s m) where
    get = State.get
    put = State.put

instance (MonadState s m, MonadTrans t, Monad (t m)) => MonadState s (t m) where
    get = lift get
    put = lift . put

gets :: MonadState s m => (s -> a) -> m a
gets f = liftM f get

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
