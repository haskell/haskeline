module System.Console.Haskeline.Command.Completion(
                            CompletionFunc,
                            Completion,
                            CompletionType(..),
                            completionCmd,
                            -- * Helper functions
                            completeWord,
                            simpleCompletion,
                            completeFilename,
                            filenameWordBreakChars,
                            ) where

import System.Console.Haskeline.Command
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Monads
import System.Console.Haskeline.InputT
import System.Console.Haskeline.Settings

import System.Directory
import System.FilePath
import Data.List(isPrefixOf, transpose, unfoldr)

makeCompletion :: Monad m => CompletionFunc m -> InsertMode -> m (InsertMode, [Completion])
makeCompletion f (IMode xs ys) = do
    (rest,completions) <- f xs
    return (IMode rest ys,completions)

-- | Create a 'Command' for word completion.
completionCmd :: Monad m => Key -> Command (InputCmdT m) InsertMode InsertMode
completionCmd k = acceptKeyM k $ \s -> Just $ do
    ctype <- asks completionType
    f <- asks complete
    let g = liftCmdT . f
    (rest,completions) <- makeCompletion g s
    case ctype of
        MenuCompletion -> return $ menuCompletion k s
                        $ map (\c -> insertString (replacement c) rest) completions
        _ -> pagingCompletion s rest completions

pagingCompletion :: Monad m => InsertMode -> InsertMode -> [Completion] 
                -> InputCmdT m (CmdAction (InputCmdT m) InsertMode)
pagingCompletion oldIM _ [] = return $ Change oldIM >=> continue
pagingCompletion _ im [newWord] 
        = return $ (Change $ insertString (replacement newWord) im) >=> continue
pagingCompletion oldIM im completions
    | oldIM /= withPartial = return $ Change withPartial >=> continue
    | otherwise = do
        layout <- ask
        let wordLines = makeLines (map display completions) layout
        prefs <- asks completionType
        return $ printWordLines prefs layout wordLines withPartial True
  where
    withPartial = insertString partial im
    partial = foldl1 commonPrefix (map replacement completions)
    commonPrefix (c:cs) (d:ds) | c == d = c : commonPrefix cs ds
    commonPrefix _ _ = ""

printWordLines :: Monad m => CompletionType -> Layout -> [String] -> InsertMode -> Bool
                -> CmdAction (InputCmdT m) InsertMode
printWordLines ctype layout wordLines im isFirst
    -- TODO: here it's assumed that it's not menu
    | usePaging ctype == False = printAll
    | otherwise = case splitAt (height layout-1) wordLines of
            (_,[]) -> printAll
            (ws,rest) -> PrintLines ws message overwrite >=> pagingCmds rest
    where
        printAll = PrintLines wordLines im overwrite >=> continue
        overwrite = not isFirst
        printOneLine [] = printAll
        printOneLine (l:ls) = PrintLines [l] message overwrite
                            >=> pagingCmds ls
        message = Message im "----More----"
        pagingCmds ls = choiceCmd [
                            acceptKeyM (KeyChar ' ') $ \_ -> Just $ return $
                                printWordLines ctype layout ls im False
                            ,acceptKey (KeyChar 'q') $ \_ -> return $
                                PrintLines [] im overwrite
                            ,acceptKeyM (KeyChar '\n') $ \_ -> Just $ return $
                                printOneLine ls
                            ]


makeLines :: [String] -> Layout -> [String]
makeLines ws layout = let
    maxLength = min (width layout) (maximum (map length ws) + 2)
    numCols = width layout `div` maxLength
    ls = if (maxLength >= width layout)
                    then map (\x -> [x]) ws
                    else splitIntoGroups numCols ws
    padToLength xs = xs ++ replicate (maxLength - length xs) ' '
    in map (concatMap padToLength) ls

-- Split xs into rows of length n,
-- such that the list increases incrementally along the columns.
-- e.g.: splitIntoGroups 4 [1..11] ==
-- [[1,4,7,10]
-- ,[2,5,8,11]
-- ,[3,6,9]]
splitIntoGroups :: Int -> [a] -> [[a]]
splitIntoGroups n xs = transpose $ unfoldr f xs
    where
        f [] = Nothing
        f ys = Just (splitAt k ys)
        k = ceilDiv (length xs) n

-- ceilDiv m n is the smallest k such that k * n >= m.
ceilDiv :: Integral a => a -> a -> a
ceilDiv m n | m `rem` n == 0    =  m `div` n
            | otherwise         =  m `div` n + 1

menuCompletion :: forall m . Monad m => Key -> InsertMode -> [InsertMode] 
                    -> CmdAction m InsertMode
menuCompletion _ oldState [] = Change oldState >=> continue
menuCompletion _ _ [c] = Change c >=> continue
menuCompletion k oldState (c:cs) = Change c >=> loop cs
    where
        loop [] = choiceCmd [change (const oldState) k,continue]
        loop (d:ds) = choiceCmd [change (const d) k >|> loop ds,continue]

--------------
-- Word break functions

-- | The following function creates a custom 'CompletionFunc' for use in the 'Settings.'
completeWord :: Monad m => Maybe Char 
        -- ^ An optional escape character
        -> String -- ^ List of characters which count as whitespace
        -> (String -> m [Completion]) -- ^ Function to produce a list of possible completions
        -> CompletionFunc m
completeWord esc ws f line = do
    let (word,rest) = case esc of
                        Nothing -> break (`elem` ws) line
                        Just e -> escapedBreak e line
    completions <- f (reverse word)
    return (rest,completions)
  where
    escapedBreak e (c:d:cs) | d == e
            = let (xs,ys) = escapedBreak e cs in (c:d:xs,ys)
    escapedBreak e (c:cs) | not (elem c ws)
            = let (xs,ys) = escapedBreak e cs in (c:xs,ys)
    escapedBreak _ cs = ("",cs)
    
-- | Adds a space after the word when inserting it after expansion.
simpleCompletion :: String -> Completion
simpleCompletion = setReplacement (++ " ") . completion

filenameWordBreakChars :: String
filenameWordBreakChars = " \t\n\\`@$><=;|&{("

-- A completion command for file and folder names.
completeFilename :: MonadIO m => CompletionFunc m
completeFilename  = completeWord (Just '\\') filenameWordBreakChars $ 
                        (liftIO . quotedFilenames (`elem` "\"\'"))

completion :: String -> Completion
completion str = Completion str str

setReplacement, setDisplay :: (String -> String) -> Completion -> Completion
setReplacement f c = c {replacement = f $ replacement c}
setDisplay f c = c {display = f $ display c}


--------
-- Helper funcs for file completion

quotedFilenames :: (Char -> Bool) -> String -> IO [Completion]
quotedFilenames isQuote (q:file) | isQuote q = do
    files <- findFiles file
    return $ map (setReplacement ((q:) . appendIfNotDir [q,' '])) files
quotedFilenames _ file = do
    files <- findFiles file
    return $ map (setReplacement (appendIfNotDir " ")) files

appendIfNotDir :: String -> FilePath -> FilePath
appendIfNotDir str file | null (takeFileName file) = file
                        | otherwise = file ++ str

addSlashToDir :: Completion -> IO Completion
addSlashToDir c@Completion{replacement=file} = do
    dirExists <- doesDirectoryExist file
    return $ if dirExists 
                then c {replacement = addTrailingPathSeparator file}
                else c

findFiles :: FilePath -> IO [Completion]
findFiles path = do
    dirExists <- doesDirectoryExist dirPath
    allFiles <- if not dirExists
        then return []
        else fmap filterPrefix $ getDirectoryContents dirPath
    let results = map (setReplacement (dir </>) . setDisplay takeFileName)
                    $ map completion $ filterPrefix allFiles
    mapM addSlashToDir results
  where
    (dir, file) = splitFileName path
    dirPath = if null dir then "." else dir
    filterPrefix = filter (\f -> not (f `elem` [".",".."])
                                        && file `isPrefixOf` f)


