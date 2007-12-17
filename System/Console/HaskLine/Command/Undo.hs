module System.Console.HaskLine.Command.Undo where

import System.Console.HaskLine.Command
import System.Console.HaskLine.LineState

data Undo = Undo {pastUndo, futureUndo :: [LineState]}
                    deriving Show

initialUndo = Undo {pastUndo = [], futureUndo = []}

type UndoT = CommandT Undo

runUndo :: Monad m => UndoT m a -> m a
runUndo f = evalCommandT initialUndo f

addToPast :: LineState -> Undo -> Undo
addToPast ls u = u {pastUndo = ls : pastUndo u, futureUndo = []}

undoPast, redoFuture :: LineState -> Undo -> (LineState,Undo)
undoPast ls u@Undo {pastUndo = []} = (ls,u) 
undoPast ls u@Undo {pastUndo = (pastLS:lss)} 
        = (pastLS, u {pastUndo = lss, futureUndo = ls : futureUndo u})

redoFuture ls u@Undo {futureUndo = []} = (ls,u) 
redoFuture ls u@Undo {futureUndo = (futureLS:lss)} 
            = (futureLS, u {futureUndo = lss, pastUndo = ls : pastUndo u})


-- want "withUndo" also...
-- should run this at start with empty LS...
saveForUndo :: MonadCmd Undo m => LineState -> m ()
saveForUndo ls = modifyState $ addToPast ls

withUndo :: MonadCmd Undo m => 
            (LineState -> m LineState) -> Command m
withUndo f = ChangeCmd $ \ls -> saveForUndo ls >> f ls

commandUndo, commandRedo :: MonadCmd Undo m => Command m
commandUndo = ChangeCmd $ updateState . undoPast

commandRedo = ChangeCmd $ updateState . redoFuture
