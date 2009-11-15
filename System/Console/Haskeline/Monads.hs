module System.Console.Haskeline.Monads(
                module Control.Monad.Trans,
                module System.Console.Haskeline.MonadException,
                ReaderT(..),
                runReaderT',
                asks,
                StateT,
                runStateT,
                evalStateT',
                gets,
                modify,
                update,
                MonadReader(..),
                MonadState(..)
                ) where

import Control.Monad.Trans
import System.Console.Haskeline.MonadException
import Prelude hiding (catch)

import Control.Monad.Reader hiding (MonadReader,ask,asks,local)
import qualified Control.Monad.Reader as Reader

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

newtype StateT s m a = StateT { getStateTFunc 
                                    :: forall r . s -> m ((a -> s -> r) -> r)}

instance Monad m => Monad (StateT s m) where
    return x = StateT $ \s -> return $ \f -> f x s
    StateT f >>= g = StateT $ \s -> do
        useX <- f s
        useX $ \x s' -> getStateTFunc (g x) s'

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        x <- m
        return $ \f -> f x s

instance MonadIO m => MonadIO (StateT s m) where
    liftIO = lift . liftIO

runStateT :: Monad m => StateT s m a -> s -> m (a, s)
runStateT f s = do
    useXS <- getStateTFunc f s
    return $ useXS $ \x s' -> (x,s')

instance Monad m => MonadState s (StateT s m) where
    get = StateT $ \s -> return $ \f -> f s s
    put s = s `seq` StateT $ \_ -> return $ \f -> f () s

instance (MonadState s m, MonadTrans t, Monad (t m)) => MonadState s (t m) where
    get = lift get
    put = lift . put

evalStateT' :: Monad m => s -> StateT s m a -> m a
evalStateT' s f = liftM fst $ runStateT f s

instance MonadException m => MonadException (StateT s m) where
    block m = StateT $ \s -> block $ getStateTFunc m s
    unblock m = StateT $ \s -> unblock $ getStateTFunc m s
    catch f h = StateT $ \s -> catch (getStateTFunc f s)
                            $ \e -> getStateTFunc (h e) s
