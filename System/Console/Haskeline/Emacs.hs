module System.Console.Haskeline.Emacs where

import System.Console.Haskeline.Command
import System.Console.Haskeline.Key
import System.Console.Haskeline.Command.Completion
import System.Console.Haskeline.Command.History
import System.Console.Haskeline.Command.Undo
import System.Console.Haskeline.LineState
import System.Console.Haskeline.InputT

import Data.Char

type InputCmd s t = forall m . Monad m => KeyCommand (InputCmdT m) s t

emacsCommands :: Monad m => KeyMap (InputCmdT m) InsertMode
emacsCommands = runCommand $ choiceCmd [simpleActions, controlActions]

simpleActions, controlActions :: InputCmd InsertMode InsertMode
simpleActions = choiceCmd 
            [ simpleChar '\n' +> finish
            , simpleKey LeftKey +> change goLeft
            , simpleKey RightKey +> change goRight
            , simpleKey Backspace +> change deletePrev
            , simpleKey Delete +> change deleteNext 
            , changeFromChar insertChar
            , saveForUndo $ completionCmd (simpleChar '\t')
            , simpleKey UpKey +> historyBack
            , simpleKey DownKey +> historyForward
            , searchHistory
            ] 
            
controlActions = choiceCmd
            [ ctrlChar 'a' +> change moveToStart 
            , ctrlChar 'e' +> change moveToEnd
            , ctrlChar 'b' +> change goLeft
            , ctrlChar 'f' +> change goRight
            , deleteCharOrEOF (ctrlChar 'd')
            , ctrlChar 'l' +> clearScreenCmd
            , metaChar 'f' +> change wordRight
            , metaChar 'b' +> change wordLeft
            , metaChar 'c' +> change (modifyWord capitalize)
            , metaChar 'l' +> change (modifyWord (mapBaseChars toLower))
            , metaChar 'u' +> change (modifyWord (mapBaseChars toUpper))
            , ctrlChar '_' +> commandUndo
            , ctrlChar 'x' +> keyCommand (try (ctrlChar 'u' +> commandUndo))
            , simpleKey Home +> change moveToStart
            , simpleKey End +> change moveToEnd
            , saveForUndo $ choiceCmd
                [ ctrlChar 'w' +> change (deleteFromMove bigWordLeft)
                , metaKey (simpleKey Backspace) +> change (deleteFromMove wordLeft)
                , metaChar 'd' +> change (deleteFromMove wordRight)
                , ctrlChar 'k' +> change (deleteFromMove moveToEnd)
                , simpleKey KillLine +> change (deleteFromMove moveToStart)
                ]
            ]

deleteCharOrEOF :: Key -> InputCmd InsertMode InsertMode
deleteCharOrEOF k = k +> askState (\s -> if s == emptyIM
                                            then failCmd
                                            else change deleteNext >|> justDelete) 
    where
        justDelete = keyCommand $ try (k +> change deleteNext >|> justDelete)

wordRight, wordLeft, bigWordLeft :: InsertMode -> InsertMode
wordRight = skipRight isAlphaNum . skipRight (not . isAlphaNum)
wordLeft = skipLeft isAlphaNum . skipLeft (not . isAlphaNum)
bigWordLeft = skipLeft (not . isSpace) . skipLeft isSpace

modifyWord :: ([Grapheme] -> [Grapheme]) -> InsertMode -> InsertMode
modifyWord f im = IMode (reverse (f ys1) ++ xs) ys2
    where
        IMode xs ys = skipRight (not . isAlphaNum) im
        (ys1,ys2) = span (isAlphaNum . baseChar) ys

capitalize :: [Grapheme] -> [Grapheme]
capitalize [] = []
capitalize (c:cs) = modifyBaseChar toUpper c : cs
