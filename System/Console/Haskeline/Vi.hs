module System.Console.Haskeline.Vi where

import System.Console.Haskeline.Command
import System.Console.Haskeline.Key
import System.Console.Haskeline.Command.Completion
import System.Console.Haskeline.Command.History
import System.Console.Haskeline.Command.KillRing
import System.Console.Haskeline.Command.Undo
import System.Console.Haskeline.LineState
import System.Console.Haskeline.InputT

import Data.Char(isAlphaNum,isSpace)

type InputCmd s t = forall m . Monad m => Command (InputCmdT m) s t
type InputKeyCmd s t = forall m . Monad m => KeyCommand (InputCmdT m) s t

viActions :: Monad m => KeyMap (InputCmdT m) InsertMode
viActions = runCommand insertionCommands

insertionCommands :: InputKeyCmd InsertMode InsertMode
insertionCommands = choiceCmd [startCommand, simpleInsertions]
                            
simpleInsertions :: InputKeyCmd InsertMode InsertMode
simpleInsertions = choiceCmd
                [ simpleChar '\n' +> finish
                   , simpleKey LeftKey +> change goLeft 
                   , simpleKey RightKey +> change goRight
                   , simpleKey Backspace +> change deletePrev 
                   , simpleKey Delete +> change deleteNext 
                   , simpleKey Home +> change moveToStart
                   , simpleKey End +> change moveToEnd
                   , changeFromChar insertChar
                   , ctrlChar 'l' +> clearScreenCmd
                   , ctrlChar 'd' +> eofIfEmpty
                   , simpleKey UpKey +> historyBack
                   , simpleKey DownKey +> historyForward
                   , searchHistory
                   , simpleKey KillLine +> killFromMove moveToStart
                   , completionCmd (simpleChar '\t')
                   ]

-- If we receive a ^D and the line is empty, return Nothing
-- otherwise, ignore it.
eofIfEmpty :: Save s => InputCmd s s
eofIfEmpty = askState $ \s -> if save s == emptyIM
                    then failCmd
                    else continue

startCommand :: InputKeyCmd InsertMode InsertMode
startCommand = simpleChar '\ESC' +> change enterCommandMode
                    >|> viCommandActions

viCommandActions :: InputCmd CommandMode InsertMode
viCommandActions = keyCommand $ simpleCmdActions `loopUntil` exitingCommands

exitingCommands :: InputKeyCmd CommandMode InsertMode
exitingCommands =  choiceCmd [ 
                      simpleChar 'i' +> change insertFromCommandMode
                    , simpleChar 'I' +> change (moveToStart . insertFromCommandMode)
                    , simpleKey Home +> change (moveToStart . insertFromCommandMode)
                    , simpleChar 'a' +> change appendFromCommandMode
                    , simpleChar 'A' +> change (moveToEnd . appendFromCommandMode)
                    , simpleKey End +> change (moveToStart  . insertFromCommandMode)
                    , simpleChar 's' +> change (insertFromCommandMode . deleteChar)
                    , simpleChar 'S' +> saveForUndo >|> change (const emptyIM)
                    -- TODO: does this work right? is the append necessary?
                    , simpleChar 'C' +> killFromMove moveToEnd
                                            >|> change appendFromCommandMode
                    , repeatedCommands
                    ]

simpleCmdActions :: InputKeyCmd CommandMode CommandMode
simpleCmdActions = choiceCmd [ simpleChar '\n'  +> finish
                    , simpleChar '\ESC' +> change id -- helps break out of loops
                    , ctrlChar 'd' +> eofIfEmpty
                    , simpleChar 'r'   +> replaceOnce 
                    , simpleChar 'R'   +> replaceLoop
                    , simpleChar 'D' +> killFromMove moveToEnd
                    , ctrlChar 'l' +> clearScreenCmd
                    , simpleChar 'u' +> commandUndo
                    , ctrlChar 'r' +> commandRedo
                    , simpleChar '.' +> commandRedo
                    -- vi-mode quirk: history is put at the start of the line.
                    , simpleChar 'j' +> historyForward >|> change moveToStart
                    , simpleChar 'k' +> historyBack >|> change moveToStart
                    , simpleKey DownKey +> historyForward  >|> change moveToStart
                    , simpleKey UpKey +> historyBack >|> change moveToStart
                    , simpleKey KillLine +> killFromMove moveToStart
                    , simpleChar 'p' +> pasteCommand pasteGraphemesAfter
                    , simpleChar 'P' +> pasteCommand pasteGraphemesBefore
                    ]

replaceOnce :: InputCmd CommandMode CommandMode
replaceOnce = try $ changeFromChar replaceChar

repeatedCommands :: InputKeyCmd CommandMode InsertMode
repeatedCommands = choiceCmd [argumented, withNoArg repeatableCommands]
    where
        withNoArg = doBefore (change (hiddenArg 1))
        start = foreachDigit startArg ['1'..'9']
        addDigit = foreachDigit addNum ['0'..'9']
        argumented = start >+> loop
        loop = keyChoiceCmd [addDigit >+> loop
                            , repeatableCommands
                            -- if no match, bail out.
                            , withoutConsuming (change argState) >+> viCommandActions
                            ]

mapMovements :: ((InsertMode -> InsertMode) -> Command m s t) -> KeyCommand m s t
mapMovements f = choiceCmd $ map (\(k,g) -> k +> f g) movements

repeatableCommands :: Monad m => KeyCommand (InputCmdT m) (ArgMode CommandMode) InsertMode
repeatableCommands = choiceCmd 
                        [ repeatableCmdToIMode
                        , repeatableCmdMode >+> viCommandActions
                        ]
                

repeatableCmdMode :: InputKeyCmd (ArgMode CommandMode) CommandMode
repeatableCmdMode = choiceCmd $ 
                    [ simpleChar 'x' +> saveForUndo >|> change (applyArg deleteChar)
                    , simpleChar 'X' +> saveForUndo >|> change (applyArg (withCommandMode deletePrev))
                    , simpleChar 'd' +> deletionCmd
                    , simpleChar 'y' +> yankCommand
                    , mapMovements (change . applyCmdArg)
                    ]

repeatableCmdToIMode :: InputKeyCmd (ArgMode CommandMode) InsertMode
repeatableCmdToIMode = simpleChar 'c' +> deletionToInsertCmd

deletionCmd :: InputCmd (ArgMode CommandMode) CommandMode
deletionCmd = keyChoiceCmd $
                    [simpleChar 'd' +> killFromArgMove (const emptyIM)
                    -- special case end-of-word because the cursor isn't in a useful position
                    -- from the motion commands.
                    , simpleChar 'e' +> killFromArgMove goToWordDelEnd
                    , simpleChar 'E' +> killFromArgMove goToBigWordDelEnd
                    , mapMovements killFromArgMove
                    , withoutConsuming (change argState)
                    ]

yankCommand :: InputCmd (ArgMode CommandMode) CommandMode
yankCommand = keyChoiceCmd $ 
                [simpleChar 'y' +> copyFromArgMove (const emptyIM)
                , mapMovements copyFromArgMove
                , withoutConsuming (change argState)
                ]

goToWordDelEnd, goToBigWordDelEnd :: InsertMode -> InsertMode
goToWordDelEnd = goRightUntil $ atStart (not . isWordChar)
                                    .||. atStart (not . isOtherChar)
goToBigWordDelEnd = goRightUntil $ atStart (not . isBigWordChar)

deletionToInsertCmd :: InputCmd (ArgMode CommandMode) InsertMode
deletionToInsertCmd = keyChoiceCmd $
        [simpleChar 'c' +> killFromArgMove (const emptyIM)
        , simpleChar 'e' +> killFromArgMove goToWordDelEnd
        , simpleChar 'E' +> killFromArgMove goToBigWordDelEnd
        -- vim,for whatever reason, treats cw same as ce and cW same as cE.
        , simpleChar 'w' +> killFromArgMove goToWordDelEnd
        , simpleChar 'W' +> killFromArgMove goToBigWordDelEnd
        -- TODO: this seems a little hacky...
        , simpleChar '$' +> killFromArgMove moveToEnd >|> change goRight
        , mapMovements killFromArgMove
        , withoutConsuming (change argState) >+> viCommandActions
        ]

movements :: [(Key,InsertMode -> InsertMode)]
movements = [ (simpleChar 'h', goLeft)
            , (simpleChar 'l', goRight)
            , (simpleChar ' ', goRight)
            , (simpleKey LeftKey, goLeft)
            , (simpleKey RightKey, goRight)
            , (simpleChar '0', moveToStart)
            , (simpleChar '$', moveToEnd)
            , (simpleChar '^', skipRight isSpace . moveToStart)
            ------------------
            -- Word movements
            -- move to the start of the next word
            , (simpleChar 'w', goRightUntil $
                                atStart isWordChar .||. atStart isOtherChar)
            , (simpleChar 'W', goRightUntil (atStart isBigWordChar))
            -- move to the beginning of the previous word
            , (simpleChar 'b', goLeftUntil $
                                atStart isWordChar .||. atStart isOtherChar)
            , (simpleChar 'B', goLeftUntil (atStart isBigWordChar))
            -- move to the end of the current word
            , (simpleChar 'e', goRightUntil $
                                atEnd isWordChar .||. atEnd isOtherChar)
            , (simpleChar 'E', goRightUntil (atEnd isBigWordChar))
            ]

{- 
From IEEE 1003.1:
A "bigword" consists of: a maximal sequence of non-blanks preceded and followed by blanks
A "word" consists of either:
 - a maximal sequence of wordChars, delimited at both ends by non-wordchars
 - a maximal sequence of non-blank non-wordchars, delimited at both ends by either blanks
   or a wordchar.
-}            
isBigWordChar, isWordChar, isOtherChar :: Char -> Bool
isBigWordChar = not . isSpace
isWordChar = isAlphaNum .||. (=='_')
isOtherChar = not . (isSpace .||. isWordChar)

(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f .||. g = \x -> f x || g x

foreachDigit :: (Monad m, LineState t) => (Int -> s -> t) -> [Char] 
                -> KeyCommand m s t
foreachDigit f ds = choiceCmd $ map digitCmd ds
    where digitCmd d = simpleChar d +> change (f (toDigit d))
          toDigit d = fromEnum d - fromEnum '0'

---------------
-- Replace mode
replaceLoop :: InputCmd CommandMode CommandMode
replaceLoop = saveForUndo >|> change insertFromCommandMode >|> loop
                >|> change enterCommandModeRight
    where
        loop = try (oneReplaceCmd >+> loop)
        oneReplaceCmd = choiceCmd [
                simpleKey LeftKey +> change goLeft
                , simpleKey RightKey +> change goRight
                , changeFromChar replaceCharIM
                ]
