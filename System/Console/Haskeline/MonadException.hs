{- | This module redefines some of the functions in "Control.Exception" to
work for more general monads than only 'IO'.
-}

module System.Console.Haskeline.MonadException(
    MonadException(..),
    handle,
    finally,
    throwIO,
    throwTo,
    bracket,
    throwDynIO,
    handleDyn,
    Exception,
    SomeException(..),
    E.IOException())
     where

import qualified Control.Exception as E
import Control.Exception (Exception,SomeException)
import Prelude hiding (catch)
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent(ThreadId)

class MonadIO m => MonadException m where
    catch :: Exception e => m a -> (e -> m a) -> m a
    block :: m a -> m a
    unblock :: m a -> m a

handle :: (MonadException m, Exception e) => (e -> m a) -> m a -> m a
handle = flip catch

finally :: MonadException m => m a -> m b -> m a
finally f ender = block (do
    r <- catch
            (unblock f)
            (\(e::SomeException) -> do {_ <- ender; throwIO e})
    _ <- ender
    return r)

throwIO :: (MonadIO m, Exception e) => e -> m a
throwIO = liftIO . E.throwIO

throwTo :: (MonadIO m, Exception e) => ThreadId -> e -> m ()
throwTo tid = liftIO . E.throwTo tid

bracket :: MonadException m => m a -> (a -> m b) -> (a -> m c) -> m c
bracket before after thing =
  block (do
    a <- before 
    r <- catch 
	   (unblock (thing a))
	   (\(e::SomeException) -> do { _ <- after a; throwIO e })
    _ <- after a
    return r
 )

throwDynIO :: (Exception exception, MonadIO m) => exception -> m a
throwDynIO = throwIO

handleDyn :: (Exception exception, MonadException m) => (exception -> m a)
                    -> m a -> m a
handleDyn = handle 


instance MonadException IO where
    catch = E.catch
    block = E.block
    unblock = E.unblock

instance MonadException m => MonadException (ReaderT r m) where
    catch f h = ReaderT $ \r -> catch (runReaderT f r) 
                            (\e -> runReaderT (h e) r)
    block = mapReaderT block
    unblock = mapReaderT unblock

-- Not needed anymore by our code (we have a custom StateT monad),
-- but we should follow the PVP and not remove this in a point release.
instance MonadException m => MonadException (StateT s m) where
    catch f h = StateT $ \s -> catch (runStateT f s)
                            (\e -> runStateT (h e) s)
    block = mapStateT block
    unblock = mapStateT unblock
