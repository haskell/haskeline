module System.Console.HaskLine.Vi where

import System.Console.HaskLine.Command
import System.Console.HaskLine.Command.Completion
import System.Console.HaskLine.Command.History
import System.Console.HaskLine.LineState
import System.Console.HaskLine.HaskLineT
import System.Console.HaskLine.Settings
import System.Console.HaskLine.Monads


type HaskLineCmd s t = forall m . MonadIO m => Command (HaskLineCmdT m) s t

viActions :: MonadIO m => KeyMap (HaskLineCmdT m) InsertMode
viActions = runCommand $ choiceCmd [startCommand, simpleInsertions]
                            
simpleInsertions :: MonadIO m => Command (HaskLineCmdT m) InsertMode InsertMode
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
                   ]

startCommand :: HaskLineCmd InsertMode InsertMode
startCommand = KeyChar '\ESC' +> change enterCommandMode
                    >|> viCommandActions

viCommandActions :: HaskLineCmd CommandMode InsertMode
viCommandActions = simpleCmdActions `loopUntil` exitingCommands

exitingCommands :: HaskLineCmd CommandMode InsertMode
exitingCommands =  choiceCmd [ KeyChar 'i' +> change insertFromCommandMode
                    , KeyChar 'I' +> change (moveToStart . insertFromCommandMode)
                    , KeyChar 'a' +> change appendFromCommandMode
                    , KeyChar 'A' +> change (moveToEnd . appendFromCommandMode)
                    , KeyChar 's' +> change (insertFromCommandMode . deleteChar)
                    , KeyChar 'S' +> change (const emptyIM)
                    , deleteIOnce
                    ]

simpleCmdActions :: HaskLineCmd CommandMode CommandMode
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

replaceOnce k = k >+> try (acceptChar replaceChar)

loopReplace k = k >+> loop
    where
        loop :: HaskLineCmd CommandMode CommandMode
        loop = loopWithBreak (acceptChar (\c -> goRight . replaceChar c))
                    (choiceCmd []) id


repeated :: HaskLineCmd CommandMode CommandMode
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
                -> HaskLineCmd s t
useMovements f = choiceCmd $ map (\(k,g) -> k +> change (f g))
                                movements

deleteOnce :: HaskLineCmd CommandMode CommandMode
deleteOnce = KeyChar 'd'
            >+> choiceCmd [useMovements deleteFromMove,
                         KeyChar 'd' +> change (const CEmpty)]

deleteIOnce :: HaskLineCmd CommandMode InsertMode
deleteIOnce = KeyChar 'c'
              >+> choiceCmd [useMovements deleteAndInsert,
                            KeyChar 'c' +> change (const emptyIM)]

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
deleteFromDiff (CMode xs1 c1 ys1) (CMode xs2 c2 ys2)
    | length xs1 < length xs2 = enterCommandMode (IMode xs1 ys2)
    | otherwise = CMode xs2 c1 ys1
deleteFromDiff _ after = after
