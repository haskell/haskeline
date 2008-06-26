module System.Console.Haskeline.Monads where

import Control.Monad(liftM)
import Control.Exception

{-- The mtl doesn't quite give us what I'd like, so I'm redoing the necessary
parts here. --}

class MonadTrans t where
    lift :: Monad m => m a -> t m a
    lift2 :: Monad m => (forall b . m b -> m b) -> t m a -> t m a

class Monad m => MonadIO m where
    liftIO :: IO a -> m a
    finallyIO :: m a -> IO b -> m a
    handleIO :: (Exception -> IO a) -> m a -> m a

instance MonadIO IO where
    liftIO = id
    finallyIO = finally
    handleIO = handle

class Monad m => MonadReader r m where
    ask :: m r
    local :: r -> m a -> m a

asks :: MonadReader r m => (r -> a) -> m a
asks f = liftM f ask

class Monad m => MonadState s m where
    get :: m s
    put :: s -> m ()

modify :: MonadState s m => (s -> s) -> m ()
modify f = get >>= put . f

update :: MonadState s m => (s -> (a,s)) -> m a
update f = do
    s <- get
    let (x,s') = f s
    put s'
    return x

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

evalReaderT :: Monad m => r -> ReaderT r m a -> m a
evalReaderT = flip runReaderT

instance Monad m => Monad (ReaderT r m) where
    return x = ReaderT $ \_ -> return x
    ReaderT f >>= g = ReaderT $ \r -> do
        x <- f r
        runReaderT (g x) r

instance MonadTrans (ReaderT r) where
    lift f = ReaderT $ \_ -> f
    lift2 f (ReaderT m) = ReaderT $ \r -> f (m r)

instance (MonadTrans t, Monad (t m), MonadReader r m) => MonadReader r (t m) where
    ask = lift ask
    local r = lift2 (local r)

instance (MonadTrans t, Monad (t m), MonadState s m) => MonadState s (t m) where
    get = lift get
    put = lift . put

instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO
    finallyIO act end = ReaderT $ \r -> finallyIO (runReaderT act r) end
    handleIO f act = ReaderT $ \r -> handleIO f (runReaderT act r)


newtype StateT s m a = StateT {runStateT :: s -> m (a,s)}

evalStateT :: Monad m => s -> StateT s m a -> m a
evalStateT s f = liftM fst $ runStateT f s

instance Monad m => Monad (StateT s m) where
    return x = StateT $ \s -> return (x,s)
    StateT f >>= g = StateT $ \s -> do
                                (x,s') <- f s
                                runStateT (g x) s'

instance Monad m => MonadReader r (ReaderT r m) where
    ask = ReaderT return
    local r (ReaderT f) = ReaderT $ \_ -> f r

instance Monad m => MonadState s (StateT s m) where
    get = StateT $ \s -> return (s,s)
    put s = StateT $ \_ -> return ((),s)

instance MonadIO m => MonadIO (StateT s m) where
    liftIO = lift . liftIO
    finallyIO act end = StateT $ \s -> finallyIO  (runStateT act s) end
    handleIO f act = StateT $ \s -> handleIO (\e -> do
                                            x <- f e
                                            return (x,s))
                                    (runStateT act s)

instance MonadTrans (StateT s) where
    lift f = StateT $ \s -> do
        x <- f
        return (x,s)
    lift2 f (StateT m) = StateT $ \s -> f (m s)


bracketSet :: (Eq a, MonadIO m) => IO a -> (a -> IO ()) -> a -> m b -> m b
bracketSet getState set newState f = do
    oldState <- liftIO getState
    if oldState == newState
        then f
        else finallyIO (liftIO (set newState) >> f) (set oldState)
