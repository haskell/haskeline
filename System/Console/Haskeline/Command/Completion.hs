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
import Control.Monad(forM)

makeCompletion :: Monad m => InsertMode -> InputCmdT m (InsertMode, [Completion])
makeCompletion (IMode xs ys) = do
    f <- asks complete
    (rest,completions) <- liftCmdT (f xs)
    return (IMode rest ys,completions)

-- | Create a 'Command' for word completion.
completionCmd :: Monad m => Key -> Command (InputCmdT m) InsertMode InsertMode
completionCmd k = k +> acceptKeyM (\s -> do
    prefs <- ask
    (rest,completions) <- makeCompletion s
    case completionType prefs of
        MenuCompletion -> return $ menuCompletion k s
                        $ map (\c -> insertString (replacement c) rest) completions
        ListCompletion -> 
                pagingCompletion prefs s rest completions k)

pagingCompletion :: Monad m => Prefs
                -> InsertMode -> InsertMode -> [Completion] 
                -> Key -> InputCmdT m (CmdAction (InputCmdT m) InsertMode)
pagingCompletion _ oldIM _ [] _ = return $ RingBell oldIM >=> continue
pagingCompletion _ _ im [newWord] _ 
        = return $ (Change $ insertString (replacement newWord) im) >=> continue
pagingCompletion prefs oldIM im completions k
    | oldIM /= withPartial = return $ Change withPartial >=> continue
    | otherwise = do
        layout <- ask
        let wordLines = makeLines (map display completions) layout
        printingCmd <- if completionPaging prefs
                        then printPage wordLines moreMessage
                        else return $ printAll wordLines withPartial
        let pageAction = askFirst (completionPromptLimit prefs) (length completions) 
                            withPartial printingCmd
        if listCompletionsImmediately prefs
            then return pageAction
            else return $ RingBell withPartial >=> 
                        try (k +> acceptKey (const pageAction))
  where
    withPartial = insertString partial im
    partial = foldl1 commonPrefix (map replacement completions)
    commonPrefix (c:cs) (d:ds) | c == d = c : commonPrefix cs ds
    commonPrefix _ _ = ""
    moreMessage = Message withPartial "----More----"

askFirst :: Monad m => Maybe Int -> Int -> InsertMode
            -> CmdAction (InputCmdT m) InsertMode
            -> CmdAction (InputCmdT m) InsertMode
askFirst mlimit numCompletions im printingCmd = case mlimit of
    Just limit | limit < numCompletions -> 
        Change (Message im ("Display all " ++ show numCompletions
                            ++ " possibilities? (y or n)"))
                    >=> choiceCmd [
                            KeyChar 'y' +> acceptKey (const printingCmd)
                            , KeyChar 'n' +> change messageState
                            ]
    _ -> printingCmd

printOneLine :: Monad m => [String] -> Message InsertMode -> CmdAction (InputCmdT m) InsertMode
printOneLine (w:ws) im | not (null ws) =
            PrintLines [w] im >=> pagingCommands ws
printOneLine _ im = Change (messageState im) >=> continue

printPage :: Monad m => [String] -> Message InsertMode
                    -> InputCmdT m (CmdAction (InputCmdT m) InsertMode)
printPage ws im = do
    layout <- ask
    return $ case splitAt (height layout - 1) ws of
        (_,[]) -> PrintLines ws (messageState im) >=> continue
        (zs,rest) -> PrintLines zs im
                    >=> pagingCommands rest


-- TODO: move testing of nullity into here
pagingCommands :: Monad m => [String] -> Command (InputCmdT m) (Message InsertMode) InsertMode
pagingCommands ws = choiceCmd [
                            KeyChar ' ' +> acceptKeyM (printPage ws)
                            ,KeyChar 'q' +> change messageState
                            ,KeyChar '\n' +> acceptKey (printOneLine ws)
                            ]


printAll :: Monad m => [String] -> InsertMode 
            -> CmdAction (InputCmdT m) InsertMode
printAll ws im = PrintLines ws im >=> continue


makeLines :: [String] -> Layout -> [String]
makeLines ws layout = let
    minColPad = 2
    printWidth = width layout
    maxLength = min printWidth (maximum (map length ws) + minColPad)
    numCols = printWidth `div` maxLength
    ls = if (maxLength >= printWidth)
                    then map (\x -> [x]) ws
                    else splitIntoGroups numCols ws
    in map (padWords maxLength) ls

-- Add spaces to the end of each word so that it takes up the given length.
-- Don't padd the word in the last column, since printing a space in the last column
-- causes a line wrap on some terminals.
padWords :: Int -> [String] -> String
padWords _ [x] = x
padWords _ [] = ""
padWords len (x:xs) = x ++ replicate (len - length x) ' '
			++ padWords len xs

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
menuCompletion _ oldState [] = RingBell oldState >=> continue
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

-- NOTE: this is the same as for readline, except that I took out the '\\'
-- so they can be used as a path separator.
filenameWordBreakChars :: String
filenameWordBreakChars = " \t\n`@$><=;|&{("

-- A completion command for file and folder names.
completeFilename :: MonadIO m => CompletionFunc m
completeFilename  = completeWord (Just '\\') filenameWordBreakChars $ 
                        (liftIO . quotedFilenames (`elem` "\"\'"))

completion :: String -> Completion
completion str = Completion str str

setReplacement :: (String -> String) -> Completion -> Completion
setReplacement f c = c {replacement = f $ replacement c}


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

findFiles :: FilePath -> IO [Completion]
-- NOTE: 'handle' catches exceptions from getDirectoryContents and getHomeDirectory.
findFiles path = handle (\_ -> return []) $ do
    fixedDir <- fixPath dir
    dirExists <- doesDirectoryExist fixedDir
    -- get all of the files in that directory, as basenames
    allFiles <- if not dirExists
                    then return []
                    else fmap (map completion . filterPrefix) 
                            $ getDirectoryContents fixedDir
    -- The replacement text should include the directory part, and also 
    -- have a trailing slash if it's itself a directory.
    forM allFiles $ \c -> do
            isDir <- doesDirectoryExist (fixedDir </> replacement c)
            return $ setReplacement (fullName . maybeAddSlash isDir) c
  where
    (dir, file) = splitFileName path
    filterPrefix = filter (\f -> not (f `elem` [".",".."])
                                        && file `isPrefixOf` f)
    maybeAddSlash False = id
    maybeAddSlash True = addTrailingPathSeparator
    -- NOTE In order for completion to work properly, all of the alternatives
    -- must have the exact same prefix.  As a result, </> is a little too clever;
    -- for example, it doesn't prepend the directory if the file looks like
    -- an absolute path (strange, but it can happen).
    -- The FilePath docs state that (++) is an exact inverse of splitFileName, so
    -- that's the right function to user here.
    fullName f = dir ++ f

-- turn a user-visible path into an internal version useable by System.FilePath.
fixPath :: String -> IO String
fixPath "" = return "."
fixPath ('~':c:path) | isPathSeparator c = do
    home <- getHomeDirectory
    return (home </> path)
fixPath path = return path
