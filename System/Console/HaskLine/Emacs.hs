module System.Console.HaskLine.Emacs where

import System.Console.HaskLine.Command
import System.Console.HaskLine.Command.Completion
import System.Console.HaskLine.Command.History
import System.Console.HaskLine.Modes

import Control.Monad.Trans

type Emacs= CommandT History
type EmacsCommand s t = forall m . MonadIO m => Command (Emacs m) s t

emacsCommands :: MonadIO m => KeyMap (Emacs m) InsertMode
emacsCommands = let actions = simpleActions actions `orKM` controlActions actions
                in actions

simpleActions, controlActions :: EmacsCommand InsertMode InsertMode
simpleActions = choiceCmd 
            [ KeyChar '\n' +> finish
            , KeyLeft +> change goLeft
            , KeyRight +> change goRight
            , Backspace +> change deletePrev
            , DeleteForward +> change deleteNext 
            , graphCommand insertChar
            , KeyChar '\t' +> fileCompletionCmd
            , KeyUp +> historyBack
            , KeyDown +> historyForward
            ] 
            
controlActions = choiceCmd
            [ controlKey 'a' +> change moveToStart 
            , controlKey 'e' +> change moveToEnd
            , controlKey 'b' +> change goLeft
            , controlKey 'c' +> change goRight
            , controlKey 'd' +> deleteCharOrEOF
            , controlKey 'l' +> clearScreen
            ]


deleteCharOrEOF :: Key -> EmacsCommand InsertMode InsertMode
deleteCharOrEOF k next = acceptKey k $ KeyAction deleteOrFail justDelete
  where
    deleteOrFail s = return $ if s == emptyIM then Fail else Change (deleteNext s)
    justDelete = (acceptKey k $ KeyAction (return . Change . deleteNext) justDelete)
                    `orKM` next
