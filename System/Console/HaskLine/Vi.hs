module System.Console.HaskLine.Vi where

import System.Console.HaskLine.Command
import System.Console.HaskLine.Command.Completion
import System.Console.HaskLine.Command.History
import System.Console.HaskLine.Modes

import Data.Function (fix)

type VIMonad = CommandT History IO

viActions :: KeyProcessor VIMonad InsertMode
viActions = startCommand `orKP` simpleInsertions
                            
simpleInsertions = choiceKP $ map ($ viActions)
                   [ KeyChar '\n' +> finishKP 
                     , KeyLeft +> changeCommand goLeft 
                     , KeyRight +> changeCommand goRight
                     , Backspace +> changeCommand deletePrev 
                     , DeleteForward +> changeCommand deleteNext 
                     , graphCommand insertChar
                     , KeyChar '\t' +> fileCompletionCmd
                     , KeyUp +> historyBack
                     , KeyDown +> historyForward
                   ]

startCommand = changeCommand enterCommandMode (KeyChar '\ESC') viCommandActions

viCommandActions :: KeyProcessor VIMonad CommandMode
viCommandActions =  exitingCommands `orKP` simpleCmdActions

exitingCommands = choiceKP $ map ($ viActions) 
                    [ KeyChar 'i' +> changeCommand insertFromCommandMode
                    , KeyChar 'I' +> changeCommand (moveToStart . insertFromCommandMode)
                    , KeyChar 'a' +> changeCommand appendFromCommandMode
                    , KeyChar 'A' +> changeCommand (moveToEnd . appendFromCommandMode)
                    , KeyChar 's' +> changeCommand (insertFromCommandMode . deleteChar)
                    , KeyChar 'S' +> changeCommand (const emptyIM)
                    ]

simpleCmdActions = choiceKP $ map ($ viCommandActions)
                    [ KeyChar '\n'  +> finishKP
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
                orKP (graphCommand replaceChar next) next
