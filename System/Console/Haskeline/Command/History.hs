module System.Console.Haskeline.Command.History where

import System.Console.Haskeline.LineState
import System.Console.Haskeline.Command
import Control.Monad(liftM,mplus)
import System.Console.Haskeline.Monads
import Data.List
import Data.Maybe(fromMaybe)
import Control.Exception(evaluate)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8

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
                    -- use binary file I/O to avoid Windows CRLF line endings
                    -- which cause confusion when switching between systems.
                    then fmap UTF8.toString (B.readFile file)
                    else return ""
    liftIO $ evaluate (length contents) -- force file closed
    let oldHistory = History (lines contents)
    (x,newHistory) <- runStateT f oldHistory
    let stifle = case stifleAmt of
                    Nothing -> id
                    Just m -> take m
    liftIO $ B.writeFile file $ UTF8.fromString 
        $ unlines $ stifle $ historyLines newHistory
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
historyBack = simpleCommand $ histUpdate prevHistory
historyForward = simpleCommand $ reverseHist $ histUpdate prevHistory

histUpdate :: MonadState HistLog m => (s -> HistLog -> (t,HistLog))
                        -> s -> m (Effect t)
histUpdate f = liftM Change . update . f

reverseHist :: MonadState HistLog m => (a -> m b) -> a -> m b
reverseHist f x = do
    modify reverser
    y <- f x
    modify reverser
    return y
  where
    reverser h = HistLog {futureHistory=pastHistory h, 
                            pastHistory=futureHistory h}

data SearchMode = SearchMode {searchTerm :: String,
                              foundHistory :: InsertMode,
                              direction :: Direction}
                        deriving Show

data Direction = Forward | Reverse
                    deriving (Show,Eq)

directionName :: Direction -> String
directionName Forward = "i-search"
directionName Reverse = "reverse-i-search"

instance LineState SearchMode where
    beforeCursor _ sm = beforeCursor prefix (foundHistory sm)
        where 
            prefix = "(" ++ directionName (direction sm) ++ ")`" 
                    ++ searchTerm sm ++ "': "
    afterCursor = afterCursor . foundHistory

instance Result SearchMode where
    toResult = toResult . foundHistory

startSearchMode :: Direction -> InsertMode -> SearchMode
startSearchMode dir im = SearchMode {searchTerm = "",foundHistory=im, direction=dir}

addChar :: Char -> SearchMode -> SearchMode
addChar c s = s {searchTerm = searchTerm s ++ [c]}

searchHistories :: Direction -> String -> [(String,HistLog)] -> Maybe (SearchMode,HistLog)
searchHistories dir text = foldr mplus Nothing . map findIt
    where
        findIt (l,h) = do 
            im <- findInLine text l
            return (SearchMode text im dir,h)

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

searchBackwards :: Bool -> SearchMode -> HistLog -> (SearchMode, HistLog)
searchBackwards useCurrent s h = let
    (text,hists) = prepSearch s h
    hists' = if useCurrent then (toResult s,h):hists else hists
    in fromMaybe (s,h) $ searchHistories (direction s) text hists'

doSearch :: MonadState HistLog m => Bool -> SearchMode -> m (Effect SearchMode)
doSearch useCurrent sm = case direction sm of
    Reverse -> histUpdate (searchBackwards useCurrent) sm
    Forward -> reverseHist (histUpdate (searchBackwards useCurrent)) sm


searchHistory :: MonadState HistLog m => Command m InsertMode InsertMode
searchHistory = choiceCmd [
                 backKey +> change (startSearchMode Reverse)
                 , forwardKey +> change (startSearchMode Forward)
                 ] >|> keepSearching
    where
        backKey = controlKey 'r'
        forwardKey = controlKey 's'
        keepSearching = choiceCmd [
                            choiceCmd [
                                charCommand oneMoreChar
                                , backKey +> simpleCommand (searchMore Reverse)
                                , forwardKey +> simpleCommand (searchMore Forward)
                                , Backspace +> change delLastChar
                                , KeyChar '\b' +> change delLastChar
                                ] >|> keepSearching
                            , changeWithoutKey foundHistory -- abort
                            ]
        delLastChar s = s {searchTerm = minit (searchTerm s)}
        minit xs = if null xs then "" else init xs
        oneMoreChar c = doSearch True . addChar c
        searchMore d s = doSearch False s {direction=d}
