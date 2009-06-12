module System.Console.Haskeline.Command.KillRing where

import System.Console.Haskeline.LineState
import System.Console.Haskeline.Command
import System.Console.Haskeline.Monads
import System.Console.Haskeline.Command.Undo
import Control.Monad

-- standard trick for a purely functional queue:
data Stack a = Stack [a] [a]
                deriving Show

emptyStack :: Stack a
emptyStack = Stack [] []

peek :: Stack a -> Maybe a
peek (Stack [] []) = Nothing
peek (Stack (x:_) _) = Just x
peek (Stack [] ys) = peek (Stack (reverse ys) [])

rotate :: Stack a -> Stack a
rotate s@(Stack [] []) = s
rotate (Stack (x:xs) ys) = Stack xs (x:ys)
rotate (Stack [] ys) = rotate (Stack (reverse ys) [])

push :: a -> Stack a -> Stack a
push x (Stack xs ys) = Stack (x:xs) ys

type KillRing = Stack [Grapheme]

runKillRing :: Monad m => StateT KillRing m a -> m a
runKillRing = evalStateT' emptyStack


pasteCommand :: (LineState s, MonadState KillRing m) => ([Grapheme] -> s -> s)
                                -> Command m s s
pasteCommand use = commandM $ do
    ms <- liftM peek get
    return $ case ms of
        Nothing -> continue
        Just s -> change (use s)

-- TODO: this should completely replace the stuff in LineState

deleteFromDiff' :: InsertMode -> InsertMode -> ([Grapheme],InsertMode)
deleteFromDiff' (IMode xs1 ys1) (IMode xs2 ys2)
    | posChange >= 0 = (take posChange ys1, IMode xs1 ys2)
    | otherwise = (take (negate posChange) ys2 ,IMode xs2 ys1)
  where
    posChange = length xs2 - length xs1

killFromMove :: (MonadState KillRing m, MonadState Undo m)
                => (InsertMode -> InsertMode) -> Command m InsertMode InsertMode
killFromMove move = saveForUndo >|> simpleCommand (\oldIM -> do
    let (gs,im) = deleteFromDiff' oldIM (move oldIM)
    modify (push gs)
    return (Change im))
