module System.Console.Haskeline.Command.History where

import System.Console.Haskeline.LineState
import System.Console.Haskeline.Command
import System.Console.Haskeline.Key
import Control.Monad(liftM,mplus)
import System.Console.Haskeline.Monads
import Data.List
import Data.Maybe(fromMaybe)
import System.Console.Haskeline.History

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
runHistoryFromFile Nothing _ f = evalStateT' emptyHistory f
runHistoryFromFile (Just file) stifleAmt f = do
    oldHistory <- liftIO $ readHistory file
    (x,newHistory) <- runStateT f (stifleHistory stifleAmt oldHistory)
    liftIO $ writeHistory file newHistory
    return x

runHistLog :: Monad m => StateT HistLog m a -> StateT History m a
runHistLog f = do
    history <- get
    lift (evalStateT' (histLog history) f)


prevHistory :: FromString s => s -> HistLog -> (s, HistLog)
prevHistory s h = let (s',h') = fromMaybe (toResult s,h) $ prevHistoryM (toResult s) h
                  in (fromString s',h')

historyBack, historyForward :: (FromString s, MonadState HistLog m) => Command m s s
historyBack = simpleCommand $ histUpdate prevHistory
historyForward = simpleCommand $ reverseHist . histUpdate prevHistory

histUpdate :: MonadState HistLog m => (s -> HistLog -> (t,HistLog))
                        -> s -> m (Effect t)
histUpdate f = liftM Change . update . f

reverseHist :: MonadState HistLog m => m b -> m b
reverseHist f = do
    modify reverser
    y <- f
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
                -- TODO will this work correctly with combining characters?
            | text `isPrefixOf` ccs = Just (IMode (stringToGraphemes prev) 
                                                (stringToGraphemes ccs))
            | otherwise = find' (c:prev) cs

prepSearch :: SearchMode -> HistLog -> (String,[(String,HistLog)])
prepSearch sm h = let
    text = searchTerm sm
    l = toResult sm
    in (text,prevHistories l h)

searchBackwards :: Bool -> SearchMode -> HistLog -> Maybe (SearchMode, HistLog)
searchBackwards useCurrent s h = let
    (text,hists) = prepSearch s h
    hists' = if useCurrent then (toResult s,h):hists else hists
    in searchHistories (direction s) text hists'

doSearch :: MonadState HistLog m => Bool -> SearchMode -> m (Effect SearchMode)
doSearch useCurrent sm = case direction sm of
    Reverse -> searchHist
    Forward -> reverseHist searchHist
  where
    searchHist = do
        hist <- get
        case searchBackwards useCurrent sm hist of
            Just (sm',hist') -> put hist' >> return (Change sm')
            Nothing -> return (RingBell sm)

searchHistory :: MonadState HistLog m => KeyCommand m InsertMode InsertMode
searchHistory = choiceCmd [
                 backKey +> change (startSearchMode Reverse)
                 , forwardKey +> change (startSearchMode Forward)
                 ] >+> keepSearching
    where
        backKey = ctrlChar 'r'
        forwardKey = ctrlChar 's'
        keepSearching = keyCommand $ choiceCmd [
                            choiceCmd [
                                charCommand oneMoreChar
                                , backKey +> simpleCommand (searchMore Reverse)
                                , forwardKey +> simpleCommand (searchMore Forward)
                                , simpleKey Backspace +> change delLastChar
                                ] >+> keepSearching
                            , withoutConsuming (change foundHistory) -- abort
                            ]
        delLastChar s = s {searchTerm = minit (searchTerm s)}
        minit xs = if null xs then "" else init xs
        oneMoreChar c = doSearch True . addChar c
        searchMore d s = doSearch False s {direction=d}
