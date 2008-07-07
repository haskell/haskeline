module System.Console.Haskeline.Command.History where

import System.Console.Haskeline.LineState
import System.Console.Haskeline.Command
import Control.Monad(liftM,mplus)
import System.Console.Haskeline.Monads
import Data.List
import Data.Maybe(fromMaybe)
import Control.Exception(evaluate)
import qualified System.IO.UTF8 as UTF8

import System.Directory(doesFileExist)

data History = History {historyLines :: [String]} -- stored in reverse

data HistLog = HistLog {pastHistory, futureHistory :: [String]}
                    deriving Show

prevHistoryM :: String -> HistLog -> Maybe (String,HistLog)
prevHistoryM _ HistLog {pastHistory = []} = Nothing
prevHistoryM s HistLog {pastHistory=ls:past, futureHistory=future}
        = Just (ls, 
            HistLog {pastHistory=past, futureHistory= s:future})

prevHistories :: String -> HistLog -> [(String,HistLog)]
prevHistories s h = flip unfoldr (s,h) $ \(s',h') -> fmap (\r -> (r,r))
                    $ prevHistoryM s' h'

histLog :: History -> HistLog
histLog hist = HistLog {pastHistory = historyLines hist, futureHistory = []}

runHistoryFromFile :: MonadIO m => Maybe FilePath -> Maybe Int -> StateT History m a -> m a
runHistoryFromFile Nothing _ f = evalStateT' (History []) f
runHistoryFromFile (Just file) stifleAmt f = do
    contents <- liftIO $ do
                exists <- doesFileExist file
                if exists
                    then UTF8.readFile file
                    else return ""
    liftIO $ evaluate (length contents) -- force file closed
    let oldHistory = History (lines contents)
    (x,newHistory) <- runStateT f oldHistory
    let stifle = case stifleAmt of
                    Nothing -> id
                    Just m -> take m
    liftIO $ UTF8.writeFile file (unlines $ stifle $ historyLines newHistory)
    return x

addHistory :: MonadState History m => String -> m ()
addHistory l = modify $ \(History ls) -> History (l:ls)

runHistLog :: Monad m => StateT HistLog m a -> StateT History m a
runHistLog f = do
    history <- get
    lift (evalStateT' (histLog history) f)


prevHistory :: FromString s => s -> HistLog -> (s, HistLog)
prevHistory s h = let (s',h') = fromMaybe (toResult s,h) $ prevHistoryM (toResult s) h
                  in (fromString s',h')

historyBack, historyForward :: (FromString s, MonadState HistLog m) => 
                        Key -> Command m s s
historyBack = simpleCommand $ liftM Change . update . prevHistory
historyForward = simpleCommand $ liftM Change . update 
                    . withReverseHist prevHistory

withReverseHist :: (s -> HistLog -> (s,HistLog)) -> s -> HistLog -> (s,HistLog)
withReverseHist f s h = let (s',h') = f s (reverseHistory h)
                        in (s',reverseHistory h')
    where
        reverseHistory h = HistLog {futureHistory=pastHistory h, 
                            pastHistory=futureHistory h}

data SearchMode = SearchMode {searchTerm :: String,
                              foundHistory :: InsertMode}
                        deriving Show

instance LineState SearchMode where
    beforeCursor _ sm = beforeCursor prefix (foundHistory sm)
        where prefix = "(reverse-i-search)`" ++ searchTerm sm ++ "': "
    afterCursor = afterCursor . foundHistory

instance Result SearchMode where
    toResult = toResult . foundHistory

startSearchMode :: InsertMode -> SearchMode
startSearchMode im = SearchMode {searchTerm = "",foundHistory=im}

addChar :: Char -> SearchMode -> SearchMode
addChar c s = s {searchTerm = searchTerm s ++ [c]}

searchHistories :: String -> [(String,HistLog)] -> Maybe (SearchMode,HistLog)
searchHistories text = foldr mplus Nothing . map findIt
    where
        findIt (l,h) = do 
            im <- findInLine text l
            return (SearchMode text im,h)

findInLine :: String -> String -> Maybe InsertMode
findInLine text l = find' [] l
    where
        find' _ "" = Nothing
        find' prev ccs@(c:cs)
            | text `isPrefixOf` ccs = Just (IMode prev ccs)
            | otherwise = find' (c:prev) cs

prepSearch :: SearchMode -> HistLog -> (String,[(String,HistLog)])
prepSearch sm h = let
    text = searchTerm sm
    l = toResult sm
    in (text,prevHistories l h)

searchHistory :: MonadState HistLog m => Command m InsertMode InsertMode
searchHistory = controlKey 'r' +> change startSearchMode >|> backSearching
    where
        backKey = controlKey 'r'
        backSearching = choiceCmd [
                            choiceCmd [
                                charCommand oneMoreChar
                                , backKey +> simpleCommand searchBackMore
                                , Backspace +> change delLastChar
                                , KeyChar '\b' +> change delLastChar
                                ] >|> backSearching
                            , changeWithoutKey foundHistory -- abort
                            ]
        delLastChar s = s {searchTerm = minit (searchTerm s)}
        minit xs = if null xs then "" else init xs
        oneMoreChar c s = do
            h <- get
            let s' = addChar c s
            let (text,hists) = prepSearch s' h
            let (s'',h') = fromMaybe (s',h) $ searchHistories
                            text ((toResult s',h):hists)
            put h'
            return $ Change s''
        searchBackMore s = do
            h <- get
            let (text,hists) = prepSearch s h
            let (s',h') = fromMaybe (s,h) $ searchHistories 
                                text hists
            put h'
            return $ Change s'
