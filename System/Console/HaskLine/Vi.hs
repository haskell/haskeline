module System.Console.HaskLine.Vi where

import System.Console.HaskLine.Command
import System.Console.HaskLine.Command.Completion
import System.Console.HaskLine.Command.History
import System.Console.HaskLine.Modes

import Control.Monad.Trans

type VI = CommandT History

viActions :: MonadIO m => KeyMap (VI m) InsertMode
viActions = let actions = startCommand actions
                    `orKM` choiceCmd actions simpleInsertions
            in actions
                            
simpleInsertions :: MonadIO m => [Command (VI m) InsertMode InsertMode]
simpleInsertions = [ KeyChar '\n' +> finish
                   , KeyLeft +> changeCommand goLeft 
                   , KeyRight +> changeCommand goRight
                   , Backspace +> changeCommand deletePrev 
                   , DeleteForward +> changeCommand deleteNext 
                   , graphCommand insertChar
                   , KeyChar '\t' +> fileCompletionCmd
                   , KeyUp +> historyBack
                   , KeyDown +> historyForward
                   ]

startCommand :: MonadIO m => Command (VI m) InsertMode InsertMode
startCommand actions = changeCommand enterCommandMode (KeyChar '\ESC') 
                        (viCommandActions actions)

viCommandActions :: MonadIO m => Command (VI m) CommandMode InsertMode
viCommandActions actions = let 
        cmdActions = choiceCmd actions exitingCommands
                        `orKM` choiceCmd cmdActions simpleCmdActions
        in cmdActions

exitingCommands :: MonadIO m => [Command (VI m) CommandMode InsertMode]
exitingCommands =   [ KeyChar 'i' +> changeCommand insertFromCommandMode
                    , KeyChar 'I' +> changeCommand (moveToStart . insertFromCommandMode)
                    , KeyChar 'a' +> changeCommand appendFromCommandMode
                    , KeyChar 'A' +> changeCommand (moveToEnd . appendFromCommandMode)
                    , KeyChar 's' +> changeCommand (insertFromCommandMode . deleteChar)
                    , KeyChar 'S' +> changeCommand (const emptyIM)
                    ]

simpleCmdActions :: MonadIO m => [Command (VI m) CommandMode CommandMode]
simpleCmdActions = [ KeyChar '\n'  +> finish
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
