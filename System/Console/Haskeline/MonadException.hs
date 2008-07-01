module System.Console.Haskeline.MonadException where

import Control.Monad
import qualified Control.Exception as E
import Prelude hiding (catch)
import Control.Exception(Exception)
import Control.Monad.Reader
import Control.Monad.State

class MonadIO m => MonadException m where
    catch :: m a -> (Exception -> m a) -> m a
    block :: m a -> m a
    unblock :: m a -> m a

handle :: MonadException m => (Exception -> m a) -> m a -> m a
handle = flip catch

finally :: MonadException m => m a -> m b -> m a
finally f ender = block (do
    r <- catch
            (unblock f)
            (\e -> do {ender; throwIO e})
    ender
    return r)

throwIO :: MonadIO m => Exception -> m a
throwIO = liftIO . throwIO

bracket :: MonadException m => m a -> (a -> m b) -> (a -> m c) -> m c
bracket before after thing =
  block (do
    a <- before 
    r <- catch 
	   (unblock (thing a))
	   (\e -> do { after a; throwIO e })
    after a
    return r
 )


instance MonadException IO where
    catch = E.catch
    block = E.block
    unblock = E.unblock

instance MonadException m => MonadException (ReaderT r m) where
    catch f h = ReaderT $ \r -> catch (runReaderT f r) 
                            (\e -> runReaderT (h e) r)
    block = mapReaderT block
    unblock = mapReaderT unblock

instance MonadException m => MonadException (StateT s m) where
    catch f h = StateT $ \s -> catch (runStateT f s)
                                (\e -> runStateT (h e) s)
    block = mapStateT block
    unblock = mapStateT unblock
