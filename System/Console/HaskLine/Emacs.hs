module System.Console.HaskLine.Emacs where

import System.Console.HaskLine.Command
import System.Console.HaskLine.Command.Completion
import System.Console.HaskLine.Command.History
import System.Console.HaskLine.Modes

import Control.Monad.Trans
import Data.Char

import System.Console.HaskLine.Vi(HaskLineT, HaskLineCmd)

emacsCommands :: MonadIO m => KeyMap (HaskLineT m) InsertMode
emacsCommands = runCommand $ choiceCmd [simpleActions, controlActions]

simpleActions, controlActions :: HaskLineCmd InsertMode InsertMode
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
            , controlKey 'l' +> clearScreenCmd
            , KeyMeta 'f' +> change (skipRight isAlphaNum
                                     . skipRight (not . isAlphaNum))
            , KeyMeta 'b' +> change (skipLeft isAlphaNum
                                     . skipLeft (not . isAlphaNum))
            ]


deleteCharOrEOF :: Key -> HaskLineCmd InsertMode InsertMode
deleteCharOrEOF k = acceptKey k deleteOrFail >|> justDelete
  where
    deleteOrFail s = return $ if s == emptyIM then Fail else Change (deleteNext s)
    justDelete = try (change deleteNext k >|> justDelete)

