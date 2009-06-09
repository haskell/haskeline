module System.Console.Haskeline.Command.Undo where

import System.Console.Haskeline.Command
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Monads

import Control.Monad


class LineState s => Save s where
    save :: s -> InsertMode
    restore :: InsertMode -> s

instance Save InsertMode where
    save = id
    restore = id

instance Save CommandMode where
    save = insertFromCommandMode
    restore = enterCommandModeRight

instance Save s => Save (ArgMode s) where
    save = save . argState
    restore = ArgMode 0 . restore


data Undo = Undo {pastUndo, futureRedo :: [InsertMode]}

type UndoT = StateT Undo

runUndoT :: Monad m => UndoT m a -> m a
runUndoT = evalStateT' initialUndo

initialUndo :: Undo
initialUndo = Undo {pastUndo = [emptyIM], futureRedo = []}


saveToUndo :: Save s => s -> Undo -> Undo
saveToUndo s undo
    | not isSame = Undo {pastUndo = toSave:pastUndo undo,futureRedo=[]}
    | otherwise = undo
  where
    toSave = save s
    isSame = case pastUndo undo of
                u:_ | u == toSave -> True
                _ -> False

undoPast, redoFuture :: Save s => s -> Undo -> (s,Undo)
undoPast ls u@Undo {pastUndo = []} = (ls,u)
undoPast ls u@Undo {pastUndo = (pastLS:lss)}
        = (restore pastLS, u {pastUndo = lss, futureRedo = save ls : futureRedo u})

redoFuture ls u@Undo {futureRedo = []} = (ls,u)
redoFuture ls u@Undo {futureRedo = (futureLS:lss)}
            = (restore futureLS, u {futureRedo = lss, pastUndo = save ls : pastUndo u})



saveForUndo :: (Save s, MonadState Undo m)
                => Command m s s
saveForUndo = askState $ \s -> commandM $ do
    modify (saveToUndo s)
    return continue

commandUndo, commandRedo :: (MonadState Undo m, Save s) => Command m s s
commandUndo = simpleCommand $ liftM Change . update . undoPast
commandRedo = simpleCommand $ liftM Change . update . redoFuture

