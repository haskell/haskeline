module System.Console.Haskeline.Command.Paste where

import System.Console.Haskeline.LineState
import System.Console.Haskeline.Command
import System.Console.Haskeline.Command.Undo

data Paste = Paste {clipboard :: Maybe String}
                deriving Show

runPaste :: Monad m => CommandT Paste m a -> m a
runPaste = evalCommandT (Paste Nothing)

paste :: LineState -> Paste -> (LineState,Paste)
paste ls p@Paste {clipboard = Nothing} = (ls,p)
paste ls p@Paste {clipboard = Just clip} = (insertText clip ls,p)

pasteCommand :: (MonadCmd Undo m, MonadCmd Paste m) => Command m
pasteCommand = withUndo $ updateState . paste

saveForPaste :: MonadCmd Paste m => String -> m ()
saveForPaste = putState . Paste . Just
