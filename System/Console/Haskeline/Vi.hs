module System.Console.Haskeline.Vi where

import System.Console.Haskeline.Command
import System.Console.Haskeline.Command.Completion
import System.Console.Haskeline.Command.History
import System.Console.Haskeline.LineState
import System.Console.Haskeline.InputT
import Control.Monad.Trans(MonadIO)

import Data.Char(isAlphaNum,isSpace)

type InputCmd s t = forall m . MonadIO m => Command (InputCmdT m) s t

viActions :: MonadIO m => KeyMap (InputCmdT m) InsertMode
viActions = runCommand insertionCommands

insertionCommands :: InputCmd InsertMode InsertMode
insertionCommands = choiceCmd [startCommand, simpleInsertions]
                            
simpleInsertions :: InputCmd InsertMode InsertMode
simpleInsertions = choiceCmd
                [ KeyChar '\n' +> finish
		   , KeyChar '\r' +> finish
                   , KeyLeft +> change goLeft 
                   , KeyRight +> change goRight
                   , Backspace +> change deletePrev 
		   , KeyChar '\b' +> change deletePrev
                   , DeleteForward +> change deleteNext 
                   , changeFromChar insertChar
                   , KeyChar '\t' +> completionCmd
                   , KeyUp +> historyBack
                   , KeyDown +> historyForward
                   , controlKey 'd' +> eofIfEmpty
                   , searchHistory
                   ]

-- If we receive a ^D and the line is empty, return Nothing
-- otherwise, ignore it.
eofIfEmpty :: Key -> InputCmd InsertMode InsertMode
eofIfEmpty k = k +> acceptKeyOrFail (\s -> if s == emptyIM
                    then Nothing
                    else Just $ Change s >=> continue)

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
                    , repeated
                    ]

simpleCmdActions :: InputCmd CommandMode CommandMode
simpleCmdActions = choiceCmd [ KeyChar '\n'  +> finish
                    , KeyChar '\ESC' +> change id -- helps break out of loops
                    , KeyChar 'r'   +> replaceOnce 
                    , KeyChar 'R'   +> loopReplace
                    , KeyChar 'x' +> change deleteChar
                    , KeyUp +> historyBack
                    , KeyDown +> historyForward
                    , deleteOnce
                    , useMovements withCommandMode
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
    deleteR = KeyChar 'd' 
                >+> choiceCmd [useMovements (deleteFromRepeatedMove),
                             KeyChar 'd' +> change (const CEmpty)]
    deleteIR = KeyChar 'c'
                >+> choiceCmd [useMovements deleteAndInsertR,
                             KeyChar 'c' +> change (const emptyIM)]
    applyArg' f am = enterCommandModeRight $ applyArg f $ fmap insertFromCommandMode am
    loop = choiceCmd [addDigit >|> loop
                     , useMovements applyArg' >|> viCommandActions
                     , deleteR >|> viCommandActions
                     , deleteIR
                     , KeyChar 'x' +> change (applyArg deleteChar)
                        >|> viCommandActions
                     , changeWithoutKey argState >|> viCommandActions
                     ]
    in start >|> loop

movements :: [(Key,InsertMode -> InsertMode)]
movements = [ (KeyChar 'h', goLeft)
            , (KeyChar 'l', goRight)
            , (KeyChar 'w', skipRight isSpace . (\s -> skipRight (cmdChar s) s))
            , (KeyChar 'b', (\s -> skipLeft (cmdChar s) s) . goLeft . skipLeft isSpace)
            , (KeyChar 'W', skipRight isSpace . skipRight (not . isSpace))
            , (KeyChar 'B', skipLeft (not . isSpace) . skipLeft isSpace)
            , (KeyChar ' ', goRight)
            , (KeyLeft, goLeft)
            , (KeyRight, goRight)
            , (KeyChar '0', moveToStart)
            , (KeyChar '$', moveToEnd)
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
deleteOnce = KeyChar 'd'
            >+> choiceCmd [useMovements deleteFromCmdMove,
                         KeyChar 'd' +> change (const CEmpty)]

deleteIOnce :: InputCmd CommandMode InsertMode
deleteIOnce = KeyChar 'c'
              >+> choiceCmd [useMovements deleteAndInsert,
                            KeyChar 'c' +> change (const emptyIM)]

deleteAndInsert :: (InsertMode -> InsertMode) -> CommandMode -> InsertMode
deleteAndInsert f = insertFromCommandMode . deleteFromCmdMove f

deleteAndInsertR :: (InsertMode -> InsertMode) 
                -> ArgMode CommandMode -> InsertMode
deleteAndInsertR f = insertFromCommandMode . deleteFromRepeatedMove f


foreachDigit :: (Monad m, LineState t) => (Int -> s -> t) -> [Char] 
                -> Command m s t
foreachDigit f ds = choiceCmd $ map digitCmd ds
    where digitCmd d = KeyChar d +> change (f (toDigit d))
          toDigit d = fromEnum d - fromEnum '0'


deleteFromCmdMove :: (InsertMode -> InsertMode) -> CommandMode -> CommandMode
deleteFromCmdMove f = withCommandMode $ \x -> deleteFromDiff x (f x)

deleteFromRepeatedMove :: (InsertMode -> InsertMode)
            -> ArgMode CommandMode -> CommandMode
deleteFromRepeatedMove f am = let
    am' = fmap insertFromCommandMode am
    in enterCommandModeRight $ 
                deleteFromDiff (argState am') (applyArg f am')
