module System.Console.Haskeline.Emacs where

import System.Console.Haskeline.Command
import System.Console.Haskeline.Command.Completion
import System.Console.Haskeline.Command.History
import System.Console.Haskeline.LineState
import System.Console.Haskeline.InputT

import Data.Char

type InputCmd s t = forall m . Monad m => Command (InputCmdT m) s t

emacsCommands :: Monad m => KeyMap (InputCmdT m) InsertMode
emacsCommands = runCommand $ choiceCmd [simpleActions, controlActions]

simpleActions, controlActions :: InputCmd InsertMode InsertMode
simpleActions = choiceCmd 
            [ KeyChar '\n' +> finish
            , KeyLeft +> change goLeft
            , KeyRight +> change goRight
            , Backspace +> change deletePrev
            , DeleteForward +> change deleteNext 
            , acceptChar insertChar
            , KeyChar '\t' +> completionCmd
            , KeyUp +> historyBack
            , KeyDown +> historyForward
            ] 
            
controlActions = choiceCmd
            [ controlKey 'a' +> change moveToStart 
            , controlKey 'e' +> change moveToEnd
            , controlKey 'b' +> change goLeft
            , controlKey 'c' +> change goRight
            , controlKey 'd' +> deleteCharOrEOF
            , controlKey 'l' +> clearScreenCmd
            , KeyMeta 'f' +> change (skipRight isAlphaNum
                                     . skipRight (not . isAlphaNum))
            , KeyMeta 'b' +> change (skipLeft isAlphaNum
                                     . skipLeft (not . isAlphaNum))
            ]

deleteCharOrEOF :: Key -> InputCmd InsertMode InsertMode
deleteCharOrEOF k = k +> acceptKeyOrFail (\s -> if s == emptyIM
            then Nothing
            else Just $ Change (deleteNext s) >=> justDelete)
    where
        justDelete = try (change deleteNext k >|> justDelete)
