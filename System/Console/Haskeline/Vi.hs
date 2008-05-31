module System.Console.Haskeline.Vi where

import System.Console.Haskeline.Command
import System.Console.Haskeline.Command.Completion
import System.Console.Haskeline.Command.History
import System.Console.Haskeline.LineState
import System.Console.Haskeline.InputT
import System.Console.Haskeline.Monads


type InputCmd s t = forall m . MonadIO m => Command (InputCmdT m) s t

viActions :: MonadIO m => KeyMap (InputCmdT m) InsertMode
viActions = runCommand $ choiceCmd [startCommand, simpleInsertions]
                            
simpleInsertions :: MonadIO m => Command (InputCmdT m) InsertMode InsertMode
simpleInsertions = choiceCmd
                [ KeyChar '\n' +> finish
                   , KeyLeft +> change goLeft 
                   , KeyRight +> change goRight
                   , Backspace +> change deletePrev 
                   , DeleteForward +> change deleteNext 
                   , acceptChar insertChar
                   , KeyChar '\t' +> completionCmd
                   , KeyUp +> historyBack
                   , KeyDown +> historyForward
                   , controlKey 'd' +> eofIfEmpty
                   ]

-- If we receive a ^D and the line is empty, return Nothing
-- otherwise, ignore it.
eofIfEmpty :: Key -> InputCmd InsertMode InsertMode
eofIfEmpty k = acceptKeyM k $ \s -> if s == emptyIM
                    then Nothing
                    else Just $ return (Change s, continue)

startCommand :: InputCmd InsertMode InsertMode
startCommand = KeyChar '\ESC' +> change enterCommandMode
                    >|> viCommandActions

viCommandActions :: InputCmd CommandMode InsertMode
viCommandActions = simpleCmdActions `loopUntil` exitingCommands

exitingCommands :: InputCmd CommandMode InsertMode
exitingCommands =  choiceCmd [ KeyChar 'i' +> change insertFromCommandMode
                    , KeyChar 'I' +> change (moveToStart . insertFromCommandMode)
                    , KeyChar 'a' +> change appendFromCommandMode
                    , KeyChar 'A' +> change (moveToEnd . appendFromCommandMode)
                    , KeyChar 's' +> change (insertFromCommandMode . deleteChar)
                    , KeyChar 'S' +> change (const emptyIM)
                    , deleteIOnce
                    ]

simpleCmdActions :: InputCmd CommandMode CommandMode
simpleCmdActions = choiceCmd [ KeyChar '\n'  +> finish
                    , KeyChar '\ESC' +> change id -- helps break out of loops
                    , KeyChar 'r'   +> replaceOnce 
                    , KeyChar 'R'   +> loopReplace
                    , KeyChar 'x' +> change deleteChar
                    , KeyUp +> historyBack
                    , KeyDown +> historyForward
                    , repeated
                    , deleteOnce
                    , useMovements id
                    ]

replaceOnce :: Key -> InputCmd CommandMode CommandMode
replaceOnce k = k >+> try (acceptChar replaceChar)

loopReplace :: Key -> InputCmd CommandMode CommandMode
loopReplace k = k >+> loop
    where
        loop = loopWithBreak (acceptChar (\c -> goRight . replaceChar c))
                    (choiceCmd []) id


repeated :: InputCmd CommandMode CommandMode
repeated = let
    start = foreachDigit startArg ['1'..'9']
    addDigit = foreachDigit addNum ['0'..'9']
    deleteR = KeyChar 'd' 
                >+> choiceCmd [useMovements (deleteFromRepeatedMove),
                             KeyChar 'd' +> change (const CEmpty)]
    deleteIR = KeyChar 'c'
                >+> choiceCmd [useMovements deleteAndInsertR,
                             KeyChar 'c' +> change (const emptyIM)]
    in start >|> loopWithBreak addDigit 
                    (choiceCmd [ useMovements applyArg
                    , deleteR
                    , spliceCmd viActions deleteIR
                    , KeyChar 'x' +> change (applyArg deleteChar)
                    ])
                    argState

movements :: [(Key,CommandMode -> CommandMode)]
movements = [ (KeyChar 'h', goLeft)
            , (KeyChar 'l', goRight)
            , (KeyChar ' ', goRight)
            , (KeyLeft, goLeft)
            , (KeyRight, goRight)
            , (KeyChar '0', moveToStart)
            , (KeyChar '$', moveToEnd)
            ]

useMovements :: LineState t => ((CommandMode -> CommandMode) -> s -> t) 
                -> InputCmd s t
useMovements f = choiceCmd $ map (\(k,g) -> k +> change (f g))
                                movements

deleteOnce :: InputCmd CommandMode CommandMode
deleteOnce = KeyChar 'd'
            >+> choiceCmd [useMovements deleteFromMove,
                         KeyChar 'd' +> change (const CEmpty)]

deleteIOnce :: InputCmd CommandMode InsertMode
deleteIOnce = KeyChar 'c'
              >+> choiceCmd [useMovements deleteAndInsert,
                            KeyChar 'c' +> change (const emptyIM)]

deleteAndInsert :: (CommandMode -> CommandMode) -> CommandMode -> InsertMode
deleteAndInsert f = insertFromCommandMode . deleteFromMove f

deleteAndInsertR :: (CommandMode -> CommandMode) 
                -> ArgMode CommandMode -> InsertMode
deleteAndInsertR f = insertFromCommandMode . deleteFromRepeatedMove f


foreachDigit :: (Monad m, LineState t) => (Int -> s -> t) -> [Char] 
                -> Command m s t
foreachDigit f ds = choiceCmd $ map digitCmd ds
    where digitCmd d = KeyChar d +> change (f (toDigit d))
          toDigit d = fromEnum d - fromEnum '0'


deleteFromMove :: (CommandMode -> CommandMode) -> CommandMode -> CommandMode
deleteFromMove f = \x -> deleteFromDiff x (f x)

deleteFromRepeatedMove :: (CommandMode -> CommandMode)
            -> ArgMode CommandMode -> CommandMode
deleteFromRepeatedMove f am = deleteFromDiff (argState am) (applyArg f am)

deleteFromDiff :: CommandMode -> CommandMode -> CommandMode
deleteFromDiff (CMode xs1 c1 ys1) (CMode xs2 _ ys2)
    | length xs1 < length xs2 = enterCommandMode (IMode xs1 ys2)
    | otherwise = CMode xs2 c1 ys1
deleteFromDiff _ after = after
