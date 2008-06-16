module System.Console.Haskeline.Term where

import System.Console.Haskeline.Monads
import System.Console.Haskeline.InputT
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Command

-- TODO: Cache the RunTerm in between runs?

class MonadTrans t => Term t where
    -- I'm suspicious of this one.
    withReposition :: MonadIO m => Layout -> t (InputCmdT m) a -> t (InputCmdT m) a
    moveToNextLine :: (MonadIO m, LineState s) => s -> t (InputCmdT m) ()
    printLines :: MonadIO m => [String] -> t m ()
    drawLineDiff :: (LineState s, LineState r, MonadIO m)
                    => String -> s -> r -> t (InputCmdT m) ()
    clearLayout :: MonadIO m => t (InputCmdT m) ()

data RunTerm t = RunTerm {
            getLayout :: IO Layout,
            withGetEvent :: MonadIO m => Bool -> (m Event -> m a) -> m a,
            runTerm :: MonadIO m => t m a -> m a
    }
