module System.Console.Haskeline.Command.History where

import System.Console.Haskeline.LineState
import System.Console.Haskeline.Command
import Control.Monad(liftM)
import System.Console.Haskeline.Monads
import Data.List
import Control.Exception(evaluate)

import System.Directory(doesFileExist)

data History = History {historyLines :: [String]} -- stored in reverse

data HistLog = HistLog {pastHistory, futureHistory :: [String]}

histLog :: History -> HistLog
histLog hist = HistLog {pastHistory = historyLines hist, futureHistory = []}

runHistoryFromFile :: MonadIO m => Maybe FilePath -> Maybe Int -> StateT History m a -> m a
runHistoryFromFile Nothing _ f = evalStateT (History []) f
runHistoryFromFile (Just file) stifleAmt f = do
    contents <- liftIO $ do
                exists <- doesFileExist file
                if exists
                    then readFile file
                    else return ""
    liftIO $ evaluate (length contents) -- force file closed
    let oldHistory = History (lines contents)
    (x,newHistory) <- runStateT f oldHistory
    let stifle = case stifleAmt of
                    Nothing -> id
                    Just m -> take m
    liftIO $ writeFile file (unlines $ stifle $ historyLines newHistory)
    return x

addHistory :: MonadState History m => String -> m ()
addHistory l = modify $ \(History ls) -> History (l:ls)

runHistLog :: Monad m => StateT HistLog m a -> StateT History m a
runHistLog f = do
    history <- get
    lift (evalStateT (histLog history) f)


prevHistory, nextHistory :: FromString s => s -> HistLog -> (s, HistLog)
prevHistory s h@HistLog {pastHistory = []} = (s,h)
prevHistory s HistLog {pastHistory=ls:past, futureHistory=future}
        = (fromString ls, 
            HistLog {pastHistory=past, futureHistory= toResult s:future})

nextHistory s h@HistLog {futureHistory = []} = (s,h)
nextHistory s HistLog {pastHistory=past, futureHistory=ls:future}
        = (fromString ls,
            HistLog {pastHistory=toResult s : past, futureHistory=future})

historyBack, historyForward :: (FromString s, MonadState HistLog m) => 
                        Key -> Command m s s
historyBack = simpleCommand $ liftM Change . update . prevHistory
historyForward = simpleCommand $ liftM Change . update . nextHistory


data SearchMode = SearchMode {searchTerm :: String,
                              foundHistory :: InsertMode}

instance LineState SearchMode where
    beforeCursor _ sm = beforeCursor prefix (foundHistory sm)
        where prefix = "(reverse-i-search)`" ++ searchTerm sm ++ "'"
    afterCursor = afterCursor . foundHistory

instance Result SearchMode where
    toResult = toResult . foundHistory

find :: String -> [String] -> Maybe ([String], String, [String])
find = undefined

findInLine :: String -> String -> Maybe InsertMode
findInLine _ "" = Nothing
findInLine text ccs@(c:cs)
    | text `isPrefixOf` ccs = Just (IMode [] ccs)
    | otherwise = fmap (insertChar c) $ findInLine text cs
