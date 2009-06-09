module System.Console.Haskeline.Vi where

import System.Console.Haskeline.Command
import System.Console.Haskeline.Key
import System.Console.Haskeline.Command.Completion
import System.Console.Haskeline.Command.History
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
                   , saveForUndo $ choiceCmd
                        [ simpleKey KillLine +> change (deleteFromMove moveToStart)
                        , completionCmd (simpleChar '\t')
                        ]
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
                    , repeated
                    , saveForUndo $ choiceCmd
                        [ simpleChar 'S' +> change (const emptyIM)
                        , deleteIOnce
                        ]
                    ]

simpleCmdActions :: InputKeyCmd CommandMode CommandMode
simpleCmdActions = choiceCmd [ simpleChar '\n'  +> finish
                    , simpleChar '\ESC' +> change id -- helps break out of loops
                    , ctrlChar 'd' +> eofIfEmpty
                    , simpleChar 'r'   +> replaceOnce 
                    , simpleChar 'R'   +> loopReplace
                    , simpleChar 'x' +> change deleteChar
                    , ctrlChar 'l' +> clearScreenCmd
                    , simpleChar 'u' +> commandUndo
                    , ctrlChar 'r' +> commandRedo
                    , simpleChar '.' +> commandRedo
                    , useMovements withCommandMode
                    -- vi-mode quirk: history is put at the start of the line.
                    , simpleChar 'j' +> historyForward >|> change moveToStart
                    , simpleChar 'k' +> historyBack >|> change moveToStart
                    , simpleKey DownKey +> historyForward  >|> change moveToStart
                    , simpleKey UpKey +> historyBack >|> change moveToStart
                    , saveForUndo $ choiceCmd
                        [ simpleKey KillLine +> change (withCommandMode
                                        $ deleteFromMove moveToStart)
                        , deleteOnce
                        ]
                    ]

replaceOnce :: InputCmd CommandMode CommandMode
replaceOnce = try $ changeFromChar replaceChar

loopReplace :: InputCmd CommandMode CommandMode
loopReplace = try $ changeFromChar (\c -> goRight . replaceChar c)
                                    >+> loopReplace

repeated :: InputKeyCmd CommandMode InsertMode
repeated = let
    start = foreachDigit startArg ['1'..'9']
    addDigit = foreachDigit addNum ['0'..'9']
    deleteR = simpleChar 'd' 
                +> keyChoiceCmd [useMovements deleteFromRepeatedMove,
                             simpleChar 'd' +> change (const CEmpty)]
    deleteIR = simpleChar 'c'
                +> keyChoiceCmd [useMovements deleteAndInsertR,
                             simpleChar 'c' +> change (const emptyIM)]
    applyArg' f = enterCommandModeRight . applyArg f . fmap insertFromCommandMode
    loop = keyChoiceCmd [addDigit >+> loop
                     , useMovements applyArg' >+> viCommandActions
                     , saveForUndo (deleteR >+> viCommandActions)
                     , saveForUndo deleteIR
                     , saveForUndo (simpleChar 'x' +> change (applyArg deleteChar)
                        >|> viCommandActions)
                     , withoutConsuming (change argState) >+> viCommandActions
                     ]
    in start >+> loop

movements :: [(Key,InsertMode -> InsertMode)]
movements = [ (simpleChar 'h', goLeft)
            , (simpleChar 'l', goRight)
            , (simpleChar 'w', skipRight isSpace . (\s -> skipRight (cmdChar s) s))
            , (simpleChar 'b', (\s -> skipLeft (cmdChar s) s) . goLeft . skipLeft isSpace)
            , (simpleChar 'W', skipRight isSpace . skipRight (not . isSpace))
            , (simpleChar 'B', skipLeft (not . isSpace) . skipLeft isSpace)
            , (simpleChar ' ', goRight)
            , (simpleKey LeftKey, goLeft)
            , (simpleKey RightKey, goRight)
            , (simpleChar '0', moveToStart)
            , (simpleChar '$', moveToEnd)
            ]

cmdChar :: InsertMode -> (Char -> Bool)
cmdChar (IMode _ (c:_))
    | isWordChar (baseChar c) = isWordChar
cmdChar _ = \d -> not (isWordChar d) && not (isSpace d)

isWordChar :: Char -> Bool
isWordChar d = isAlphaNum d || d == '_'

useMovements :: LineState t => ((InsertMode -> InsertMode) -> s -> t) 
                -> InputKeyCmd s t
useMovements f = choiceCmd $ map (\(k,g) -> k +> change (f g))
                                movements

deleteOnce :: InputKeyCmd CommandMode CommandMode
deleteOnce = simpleChar 'd'
            +> keyChoiceCmd [useMovements deleteFromCmdMove,
                         simpleChar 'd' +> change (const CEmpty)]

deleteIOnce :: InputKeyCmd CommandMode InsertMode
deleteIOnce = simpleChar 'c'
              +> keyChoiceCmd [useMovements deleteAndInsert,
                            simpleChar 'c' +> change (const emptyIM)]

deleteAndInsert :: (InsertMode -> InsertMode) -> CommandMode -> InsertMode
deleteAndInsert f = insertFromCommandMode . deleteFromCmdMove f

deleteAndInsertR :: (InsertMode -> InsertMode) 
                -> ArgMode CommandMode -> InsertMode
deleteAndInsertR f = insertFromCommandMode . deleteFromRepeatedMove f


foreachDigit :: (Monad m, LineState t) => (Int -> s -> t) -> [Char] 
                -> KeyCommand m s t
foreachDigit f ds = choiceCmd $ map digitCmd ds
    where digitCmd d = simpleChar d +> change (f (toDigit d))
          toDigit d = fromEnum d - fromEnum '0'


deleteFromCmdMove :: (InsertMode -> InsertMode) -> CommandMode -> CommandMode
deleteFromCmdMove f = withCommandMode $ \x -> deleteFromDiff x (f x)

deleteFromRepeatedMove :: (InsertMode -> InsertMode)
            -> ArgMode CommandMode -> CommandMode
deleteFromRepeatedMove f am = let
    am' = fmap insertFromCommandMode am
    in enterCommandModeRight $ 
                deleteFromDiff (argState am') (applyArg f am')
