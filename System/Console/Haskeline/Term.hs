module System.Console.Haskeline.Term where

import System.Console.Haskeline.Monads
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Command

-- TODO: Cache the RunTerm in between runs?
-- If do this, should make sure in Terminfo and dumb terms that they 
-- cache the input keymaps too.

class MonadIO m => Term m where
    withReposition :: Layout -> m a -> m a
    moveToNextLine :: LineState s => s -> m ()
    printLines :: [String] -> m ()
    drawLineDiff :: (LineState s, LineState r)
                    => String -> s -> r -> m ()
    clearLayout :: m ()
    

data RunTerm t m = RunTerm {
            getLayout :: IO Layout,
            withGetEvent :: forall a . Bool -> (t m Event -> t m a) -> t m a,
            runTerm :: forall a . t m a -> m a
    }

-- Utility function for drawLineDiff instances.
matchInit :: Eq a => [a] -> [a] -> ([a],[a])
matchInit (x:xs) (y:ys)  | x == y = matchInit xs ys
matchInit xs ys = (xs,ys)


