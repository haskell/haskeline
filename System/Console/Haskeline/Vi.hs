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

viActions :: Monad m => KeyMap (InputCmdT m) InsertMode
viActions = runCommand insertionCommands

insertionCommands :: InputCmd InsertMode InsertMode
insertionCommands = choiceCmd [startCommand, simpleInsertions]
                            
simpleInsertions :: InputCmd InsertMode InsertMode
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
                        , simpleChar '\t' +> completionCmd
                        ]
                   ]

-- If we receive a ^D and the line is empty, return Nothing
-- otherwise, ignore it.
eofIfEmpty :: Save s => Key -> InputCmd s s
eofIfEmpty k = k +> acceptKeyOrFail (\s -> if save s == emptyIM
                    then Nothing
                    else Just $ Change s >=> continue)

startCommand :: InputCmd InsertMode InsertMode
startCommand = simpleChar '\ESC' +> change enterCommandMode
                    >|> viCommandActions

viCommandActions :: InputCmd CommandMode InsertMode
viCommandActions = simpleCmdActions `loopUntil` exitingCommands

exitingCommands :: InputCmd CommandMode InsertMode
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

simpleCmdActions :: InputCmd CommandMode CommandMode
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
                    , simpleKey DownKey +> historyForward
                    , simpleKey UpKey +> historyBack
                    , saveForUndo $ choiceCmd
                        [ simpleKey KillLine +> change (withCommandMode
                                        $ deleteFromMove moveToStart)
                        , deleteOnce
                        ]
                    ]

replaceOnce :: Key -> InputCmd CommandMode CommandMode
replaceOnce k = k >+> try (changeFromChar replaceChar)

loopReplace :: Key -> InputCmd CommandMode CommandMode
loopReplace k = k >+> loop
    where
        loop = choiceCmd [changeFromChar (\c -> goRight . replaceChar c) >|> loop
                         , continue]

repeated :: InputCmd CommandMode InsertMode
repeated = let
    start = foreachDigit startArg ['1'..'9']
    addDigit = foreachDigit addNum ['0'..'9']
    deleteR = simpleChar 'd' 
                >+> choiceCmd [useMovements (deleteFromRepeatedMove),
                             simpleChar 'd' +> change (const CEmpty)]
    deleteIR = simpleChar 'c'
                >+> choiceCmd [useMovements deleteAndInsertR,
                             simpleChar 'c' +> change (const emptyIM)]
    applyArg' f am = enterCommandModeRight $ applyArg f $ fmap insertFromCommandMode am
    loop = choiceCmd [addDigit >|> loop
                     , useMovements applyArg' >|> viCommandActions
                     , saveForUndo (deleteR >|> viCommandActions)
                     , saveForUndo deleteIR
                     , saveForUndo (simpleChar 'x' +> change (applyArg deleteChar)
                        >|> viCommandActions)
                     , changeWithoutKey argState >|> viCommandActions
                     ]
    in start >|> loop

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
    | isWordChar c = isWordChar
cmdChar _ = \d -> not (isWordChar d) && not (isSpace d)

isWordChar :: Char -> Bool
isWordChar d = isAlphaNum d || d == '_'

useMovements :: LineState t => ((InsertMode -> InsertMode) -> s -> t) 
                -> InputCmd s t
useMovements f = choiceCmd $ map (\(k,g) -> k +> change (f g))
                                movements

deleteOnce :: InputCmd CommandMode CommandMode
deleteOnce = simpleChar 'd'
            >+> choiceCmd [useMovements deleteFromCmdMove,
                         simpleChar 'd' +> change (const CEmpty)]

deleteIOnce :: InputCmd CommandMode InsertMode
deleteIOnce = simpleChar 'c'
              >+> choiceCmd [useMovements deleteAndInsert,
                            simpleChar 'c' +> change (const emptyIM)]

deleteAndInsert :: (InsertMode -> InsertMode) -> CommandMode -> InsertMode
deleteAndInsert f = insertFromCommandMode . deleteFromCmdMove f

deleteAndInsertR :: (InsertMode -> InsertMode) 
                -> ArgMode CommandMode -> InsertMode
deleteAndInsertR f = insertFromCommandMode . deleteFromRepeatedMove f


foreachDigit :: (Monad m, LineState t) => (Int -> s -> t) -> [Char] 
                -> Command m s t
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
