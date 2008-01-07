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
                   , KeyLeft +> changeCommand goLeft 
                   , KeyRight +> changeCommand goRight
                   , Backspace +> changeCommand deletePrev 
                   , DeleteForward +> changeCommand deleteNext 
                   , graphCommand insertChar
                   , KeyChar '\t' +> fileCompletionCmd
                   , KeyUp +> historyBack
                   , KeyDown +> historyForward
                   ]

startCommand :: VICommand InsertMode InsertMode
startCommand actions = changeCommand enterCommandMode (KeyChar '\ESC') 
                        (viCommandActions actions)

viCommandActions :: VICommand CommandMode InsertMode
viCommandActions actions = let 
        cmdActions = exitingCommands actions 
                        `orKM` simpleCmdActions cmdActions 
        in cmdActions

exitingCommands :: VICommand CommandMode InsertMode
exitingCommands =  choiceCmd [ KeyChar 'i' +> changeCommand insertFromCommandMode
                    , KeyChar 'I' +> changeCommand (moveToStart . insertFromCommandMode)
                    , KeyChar 'a' +> changeCommand appendFromCommandMode
                    , KeyChar 'A' +> changeCommand (moveToEnd . appendFromCommandMode)
                    , KeyChar 's' +> changeCommand (insertFromCommandMode . deleteChar)
                    , KeyChar 'S' +> changeCommand (const emptyIM)
                    ]

simpleCmdActions :: VICommand CommandMode CommandMode
simpleCmdActions = choiceCmd [ KeyChar '\n'  +> finish
                    , KeyChar '0'   +> changeCommand moveToStart
                    , KeyChar '$'   +> changeCommand moveToEnd
                    , KeyChar 'r'   +> replaceOnce 
                    , KeyLeft       +> changeCommand goLeft
                    , KeyRight      +> changeCommand goRight
                    , KeyChar ' '   +> changeCommand goRight
                    , KeyChar 'x'   +> changeCommand deleteChar
                    , KeyUp +> historyBack
                    , KeyDown +> historyForward
                    ]

replaceOnce k next = acceptKey k $ nullAction $
                orKM (graphCommand replaceChar next) next
