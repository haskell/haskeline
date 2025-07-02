-- | Provides an interface for 'ReaderT' rather than 'InputT'.
--
-- @since 0.8.4.0
module System.Console.Haskeline.ReaderT
    ( -- * ReaderT
      -- $reader
      toReaderT,
      fromReaderT,
      InputTEnv,
      mapInputTEnv,
    ) where

import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT))
import Data.IORef (IORef)
import System.Console.Haskeline.Command.KillRing (KillRing)
import System.Console.Haskeline.History (History)
import System.Console.Haskeline.InputT(
                         InputT(InputT),
                         Settings(
                            Settings,
                            autoAddHistory,
                            complete,
                            historyFile
                            )
                         )
import System.Console.Haskeline.Prefs (Prefs)
import System.Console.Haskeline.Term (RunTerm)


-- $reader
--
-- These functions expose the 'InputT' type in terms of ordinary 'ReaderT'.
-- This allows for easier integration with applications that do not use a
-- concrete monad transformer stack, or do not want to commit to having
-- 'InputT' in the stack.

-- | The abstract environment used by 'InputT', for 'ReaderT'
-- usage.
--
-- @since 0.8.4.0
data InputTEnv m = MkInputTEnv
    { runTerm :: RunTerm,
      history :: IORef History,
      killRing :: IORef KillRing,
      prefs :: Prefs,
      settings :: Settings m
    }

-- | Maps the environment.
--
-- @since 0.8.4.0
mapInputTEnv :: (forall x. m x -> n x) -> InputTEnv m -> InputTEnv n
mapInputTEnv f (MkInputTEnv t h k p s) =
    MkInputTEnv
        { runTerm = t,
          history = h,
          killRing = k,
          prefs = p,
          settings = settings'
        }
    where
        settings' = Settings
            { complete = f . complete s,
              historyFile = historyFile s,
              autoAddHistory = autoAddHistory s
            }

-- | Maps an 'InputT' to a 'ReaderT'. Useful for lifting 'InputT' functions
-- into some other reader-like monad.
--
-- __Examples:__
--
-- @
-- import System.Console.Haskeline qualified as H
--
-- runApp :: ReaderT (InputTEnv IO) IO ()
-- runApp = do
--   input <- toReaderT (H.getInputLine "Enter your name: ")
--   ...
-- @
--
-- @
-- -- MTL-style polymorphism
-- class MonadHaskeline m where
--   getInputLine :: String -> m (Maybe String)
--
-- -- AppT is the core type over ReaderT.
-- instance MonadHaskeline (AppT m) where
--   getInputLine = lift . toReaderT . H.getInputLine
--
-- runApp :: (MonadHaskeline m) => m ()
-- runApp = do
--   input <- getInputLine "Enter your name: "
--   ...
-- @
--
-- @since 0.8.4.0
toReaderT :: InputT m a -> ReaderT (InputTEnv m) m a
toReaderT (InputT rdr) = ReaderT $ \st ->
    usingReaderT (settings st)
        . usingReaderT (prefs st)
        . usingReaderT (killRing st)
        . usingReaderT (history st)
        . usingReaderT (runTerm st)
        $ rdr

-- | Maps a 'ReaderT' to an 'InputT'. Allows defining an application in terms
-- of 'ReaderT'.
--
-- __Examples:__
--
-- @
-- import System.Console.Haskeline qualified as H
--
-- -- Could be generalized to MonadReader, effects libraries.
-- runApp :: ReaderT (InputTEnv IO) IO ()
--
-- main :: IO ()
-- main = H.runInputT H.defaultSettings $ fromReaderT runApp
-- @
--
-- @since 0.8.4.0
fromReaderT :: ReaderT (InputTEnv m) m a -> InputT m a
fromReaderT (ReaderT onEnv) = InputT $ ReaderT
    $ \runTerm' -> ReaderT
    $ \history' -> ReaderT
    $ \killRing' -> ReaderT
    $ \prefs' -> ReaderT
    $ \settings' ->
        onEnv (MkInputTEnv runTerm' history' killRing' prefs' settings')

usingReaderT :: env -> ReaderT env m a -> m a
usingReaderT = flip runReaderT
