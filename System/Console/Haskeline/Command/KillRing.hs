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


pasteCommand :: (Save s, MonadState KillRing m, MonadState Undo m)
            => ([Grapheme] -> s -> s) -> Command m s s
pasteCommand use = simpleCommand $ \s -> do
    ms <- liftM peek get
    case ms of
        Nothing -> return $ Change s
        Just p -> do
            modify (saveToUndo s)
            return $ Change $ use p s

-- TODO: this first case isn't really great...
deleteFromDiff' :: InsertMode -> InsertMode -> ([Grapheme],InsertMode)
deleteFromDiff' (IMode xs ys) im@(IMode [] []) = (reverse xs ++ ys,im)
deleteFromDiff' (IMode xs1 ys1) (IMode xs2 ys2)
    | posChange >= 0 = (take posChange ys1, IMode xs1 ys2)
    | otherwise = (take (negate posChange) ys2 ,IMode xs2 ys1)
  where
    posChange = length xs2 - length xs1

killFromMove :: (MonadState KillRing m, MonadState Undo m,
                        Save s, Save t)
                => (InsertMode -> InsertMode) -> Command m s t
killFromMove move = saveForUndo >|> simpleCommand (\oldS -> do
    let oldIM = save oldS
    let (gs,newIM) = deleteFromDiff' oldIM (move oldIM)
    modify (push gs)
    return (Change (restore newIM)))

killFromArgMove :: (MonadState KillRing m, MonadState Undo m, Save s, Save t)
                => (InsertMode -> InsertMode) -> Command m (ArgMode s) t
killFromArgMove move = saveForUndo >|> simpleCommand (\oldS -> do
    let oldIMA = fmap save oldS
    let (gs,newIM) = deleteFromDiff' (argState oldIMA) (applyArg move oldIMA)
    modify (push gs)
    return (Change (restore newIM)))

copyFromArgMove :: (MonadState KillRing m, Save s)
                => (InsertMode -> InsertMode) -> Command m (ArgMode s) s
copyFromArgMove move = simpleCommand $ \oldS -> do
    let oldIMA = fmap save oldS
    let (gs,_) = deleteFromDiff' (argState oldIMA) (applyArg move oldIMA)
    modify (push gs)
    return $ Change $ argState oldS
