module System.Console.Haskeline.Emacs where

import System.Console.Haskeline.Command
import System.Console.Haskeline.Key
import System.Console.Haskeline.Command.Completion
import System.Console.Haskeline.Command.History
import System.Console.Haskeline.Command.Undo
import System.Console.Haskeline.LineState
import System.Console.Haskeline.InputT

import Data.Char

type InputCmd s t = forall m . Monad m => Command (InputCmdT m) s t

emacsCommands :: Monad m => KeyMap (InputCmdT m) InsertMode
emacsCommands = runCommand $ choiceCmd [simpleActions, controlActions]

simpleActions, controlActions :: InputCmd InsertMode InsertMode
simpleActions = choiceCmd 
            [ simpleKey Return +> finish
            , simpleKey LeftKey +> change goLeft
            , simpleKey RightKey +> change goRight
            , simpleKey Backspace +> change deletePrev
            , simpleKey Delete +> change deleteNext 
            , changeFromChar insertChar
            , saveForUndo $ simpleKey Tab +> completionCmd
            , simpleKey UpKey +> historyBack
            , simpleKey DownKey +> historyForward
            , searchHistory
            ] 
            
controlActions = choiceCmd
            [ ctrlChar 'a' +> change moveToStart 
            , ctrlChar 'e' +> change moveToEnd
            , ctrlChar 'b' +> change goLeft
            , ctrlChar 'f' +> change goRight
            , ctrlChar 'd' +> deleteCharOrEOF
            , ctrlChar 'l' +> clearScreenCmd
            , metaChar 'f' +> change wordRight
            , metaChar 'b' +> change wordLeft
            , ctrlChar '_' +> commandUndo
            , ctrlChar 'x' +> change id 
            , simpleKey Home +> change moveToStart
            , simpleKey End +> change moveToEnd
                >|> choiceCmd [ctrlChar 'u' +> commandUndo
                              , continue]
            , saveForUndo $ choiceCmd
                [ ctrlChar 'w' +> change (deleteFromMove bigWordLeft)
                , Key (Just Meta) Backspace +> change (deleteFromMove wordLeft)
                , metaChar 'd' +> change (deleteFromMove wordRight)
                , ctrlChar 'k' +> change (deleteFromMove moveToEnd)
                , simpleKey KillLine +> change (deleteFromMove moveToStart)
                ]
            ]

deleteCharOrEOF :: Key -> InputCmd InsertMode InsertMode
deleteCharOrEOF k = k +> acceptKeyOrFail (\s -> if s == emptyIM
            then Nothing
            else Just $ Change (deleteNext s) >=> justDelete)
    where
        justDelete = try (change deleteNext k >|> justDelete)

wordRight, wordLeft, bigWordLeft :: InsertMode -> InsertMode
wordRight = skipRight isAlphaNum . skipRight (not . isAlphaNum)
wordLeft = skipLeft isAlphaNum . skipLeft (not . isAlphaNum)
bigWordLeft = skipLeft (not . isSpace) . skipLeft isSpace
