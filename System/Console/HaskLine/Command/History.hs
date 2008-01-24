module System.Console.HaskLine.Command.History where

import System.Console.HaskLine.LineState
import System.Console.HaskLine.Modes
import System.Console.HaskLine.Command
import Control.Monad (liftM)
import Data.List

data History = History {pastHistory, futureHistory :: [String]}

-- todo: func that wraps runHSLine and saves it in history.
-- need other monad for that.
runHistory :: Monad m => [String] -> CommandT History m a -> m a
runHistory past = evalCommandT History {pastHistory=past, futureHistory=[]}

prevHistory, nextHistory :: FromString s => s -> History -> (s, History)
prevHistory s h@History {pastHistory = []} = (s,h)
prevHistory s History {pastHistory=ls:past, futureHistory=future}
        = (fromString ls, 
            History {pastHistory=past, futureHistory= toResult s:future})

nextHistory s h@History {futureHistory = []} = (s,h)
nextHistory s History {pastHistory=past, futureHistory=ls:future}
        = (fromString ls,
            History {pastHistory=toResult s : past, futureHistory=future})

historyBack, historyForward :: (FromString s, MonadCmd History m) => 
                        Key -> Command m s s
historyBack = simpleCommand $ liftM Change . updateState . prevHistory
historyForward = simpleCommand $ liftM Change . updateState . nextHistory


data SearchMode = SearchMode {searchTerm :: String,
                              foundHistory :: InsertMode}

instance LineState SearchMode where
    beforeCursor _ sm = beforeCursor prefix (foundHistory sm)
        where prefix = "(reverse-i-search)`" ++ searchTerm sm ++ "'"
    afterCursor = afterCursor . foundHistory
    toResult = toResult . foundHistory

find :: String -> [String] -> Maybe ([String], String, [String])
find = undefined

findInLine :: String -> String -> Maybe InsertMode
findInLine text "" = Nothing
findInLine text ccs@(c:cs)
    | text `isPrefixOf` ccs = Just (IMode [] ccs)
    | otherwise = fmap (insertChar c) $ findInLine text cs
