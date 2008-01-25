module System.Console.HaskLine.Command.History where

import System.Console.HaskLine.LineState
import System.Console.HaskLine.Modes
import System.Console.HaskLine.Command
import Control.Monad(liftM)
import Control.Monad.Trans
import Data.List
import Control.Exception(evaluate)

import System.Directory(doesFileExist)

data History = History {historyLines :: [String]} -- stored in reverse

data HistLog = HistLog {pastHistory, futureHistory :: [String]}

histLog :: History -> HistLog
histLog hist = HistLog {pastHistory = historyLines hist, futureHistory = []}

runHistoryFromFile :: MonadIO m => FilePath -> CommandT History m a -> m a
runHistoryFromFile file f = do
    contents <- liftIO $ do
                exists <- doesFileExist file
                if exists
                    then readFile file
                    else return ""
    liftIO $ evaluate (length contents) -- force file closed
    let oldHistory = History (lines contents)
    (x,newHistory) <- runCommandT f oldHistory
    liftIO $ writeFile file (unlines $ historyLines newHistory)
    return x

addHistory :: Monad m => String -> CommandT History m ()
addHistory l = modifyState $ \(History ls) -> History (l:ls)

runHistLog :: Monad m => CommandT HistLog m a -> CommandT History m a
runHistLog f = do
    history <- getState
    lift (evalCommandT (histLog history) f)


prevHistory, nextHistory :: FromString s => s -> HistLog -> (s, HistLog)
prevHistory s h@HistLog {pastHistory = []} = (s,h)
prevHistory s HistLog {pastHistory=ls:past, futureHistory=future}
        = (fromString ls, 
            HistLog {pastHistory=past, futureHistory= toResult s:future})

nextHistory s h@HistLog {futureHistory = []} = (s,h)
nextHistory s HistLog {pastHistory=past, futureHistory=ls:future}
        = (fromString ls,
            HistLog {pastHistory=toResult s : past, futureHistory=future})

historyBack, historyForward :: (FromString s, MonadCmd HistLog m) => 
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
findInLine _ "" = Nothing
findInLine text ccs@(c:cs)
    | text `isPrefixOf` ccs = Just (IMode [] ccs)
    | otherwise = fmap (insertChar c) $ findInLine text cs
