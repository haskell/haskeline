module System.Console.HaskLine.Command.History where

import System.Console.HaskLine.LineState
import System.Console.HaskLine.Command
import System.Console.HaskLine.Command.Undo

data History = History {pastHistory, futureHistory :: [String]}

-- todo: func that wraps runHSLine and saves it in history.
-- need other monad for that.
runHistory :: Monad m => [String] -> CommandT History m a -> m a
runHistory past = evalCommandT History {pastHistory=past, futureHistory=[]}

prevHistory, nextHistory :: LineState -> History -> (LineState,History)
prevHistory ls h@History {pastHistory = []} = (ls,h)
prevHistory ls History {pastHistory=ls':past, futureHistory=future}
        = (moveToEnd (lineState ls'), 
            History {pastHistory=past, futureHistory=lineContents ls:future})

nextHistory ls h@History {futureHistory = []} = (ls,h)
nextHistory ls History {pastHistory=past, futureHistory=ls':future}
        = (moveToEnd (lineState ls'), 
            History {pastHistory=lineContents ls : past, futureHistory=future})

historyBack, historyForward :: MonadCmd History m => Command m
historyBack = changeCommand $ updateState . prevHistory
historyForward = changeCommand $ updateState . nextHistory

