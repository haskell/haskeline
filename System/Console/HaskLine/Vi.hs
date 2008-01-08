module System.Console.HaskLine.Vi where

import System.Console.HaskLine.Command
import System.Console.HaskLine.Command.Completion
import System.Console.HaskLine.Command.History
import System.Console.HaskLine.Modes

import Control.Monad.Trans

type VI = CommandT History
type VICommand s t = forall m . MonadIO m => Command (VI m) s t

viActions :: MonadIO m => KeyMap (VI m) InsertMode
viActions = let actions = startCommand actions
                    `orKM` simpleInsertions actions
            in actions
                            
simpleInsertions :: VICommand InsertMode InsertMode
simpleInsertions = choiceCmd
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

startCommand :: VICommand InsertMode InsertMode
startCommand actions = change enterCommandMode (KeyChar '\ESC') 
                        (viCommandActions actions)

viCommandActions :: VICommand CommandMode InsertMode
viCommandActions actions = let 
        cmdActions = exitingCommands actions 
                        `orKM` simpleCmdActions cmdActions 
        in cmdActions

exitingCommands :: VICommand CommandMode InsertMode
exitingCommands =  choiceCmd [ KeyChar 'i' +> change insertFromCommandMode
                    , KeyChar 'I' +> change (moveToStart . insertFromCommandMode)
                    , KeyChar 'a' +> change appendFromCommandMode
                    , KeyChar 'A' +> change (moveToEnd . appendFromCommandMode)
                    , KeyChar 's' +> change (insertFromCommandMode . deleteChar)
                    , KeyChar 'S' +> change (const emptyIM)
                    ]

simpleCmdActions :: VICommand CommandMode CommandMode
simpleCmdActions = choiceCmd [ KeyChar '\n'  +> finish
                    , KeyChar '0'   +> change moveToStart
                    , KeyChar '$'   +> change moveToEnd
                    , KeyChar 'r'   +> replaceOnce 
                    , KeyLeft       +> change goLeft
                    , KeyRight      +> change goRight
                    , KeyChar ' '   +> change goRight
                    , KeyChar 'x'   +> change deleteChar
                    , KeyUp +> historyBack
                    , KeyDown +> historyForward
                    ]

replaceOnce k next = acceptKey k $ nullAction $
                orKM (graphCommand replaceChar next) next
