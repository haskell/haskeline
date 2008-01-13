module System.Console.HaskLine.Command.Completion(
                            CompletionFunc,
                            Expansion(..),
                            makeExpansion,
                            WordBreakFunc,
                            simpleWordBreak,
                            completionCmd,
                            -- * Filename completion
                            completeFile,
                            filenameWordBreak,
                            fileCompletionCmd,
                            -- * Helper functions
                            basicCompletionFunc,
                            addSpaceIfDone,
                            quoteCompletion
                            ) where

import System.Console.HaskLine.Command
import System.Console.HaskLine.Modes
import System.Console.HaskLine.LineState

import System.Directory
import System.FilePath
import Control.Monad(liftM)
import Control.Monad.Trans
import Data.List(isPrefixOf, transpose, unfoldr)

-- | An action which completes a word; for example, expanding the first few
-- letters of a filename into the full filename.
type CompletionFunc m = String -> m Expansion

-- The result of a word completion.
data Expansion = NoExpansion | FullExpansion String 
                    | Partial {partialExpansion :: String,
                                potentialCompletions :: [String]}
                    deriving Show

-- | Convert a list of potential word expansions into an 'Expansion'.
makeExpansion :: [String] -> Expansion
makeExpansion [] = NoExpansion
makeExpansion [s] = FullExpansion s
makeExpansion ss = Partial (commonPrefix ss) ss
    where
        commonPrefix :: [String] -> String
        commonPrefix [] = ""
        commonPrefix ts = foldl1 common ts
        common _ "" = ""
        common "" _ = ""
        common (c:cs) (d:ds)
            | c == d = c : common cs ds
            | otherwise = ""


-- | Break off a reversed word from a reversed string.  The input and output 'String's are reversed.
-- 
-- For example, @break (==' ')@ is a 'WordBreakFunc':
--
-- >  break (==' ') (reverse "This is a sentence") == ("ecnetnes"," a si sihT")
-- 
type WordBreakFunc = String -> (String, String)

-- | Create a 'Command' for word completion.
completionCmd :: Monad m => WordBreakFunc -> CompletionFunc m 
    -> Key -> Command m InsertMode InsertMode
completionCmd breakWord complete key = acceptKey key $ \s@(IMode xs ys) -> do
    let (revWord,rest) = breakWord xs
    let word = reverse revWord
    expansion <- complete word
    let addExpanded xs' = IMode (reverse xs' ++ rest) ys
    return $ case expansion of
        NoExpansion -> Change s
        FullExpansion newWord -> Change $ addExpanded newWord
        Partial partial ws 
            | length ws > 1 && partial == word
                                -> PrintLines (makeLines ws) $ addExpanded partial
            | otherwise         -> Change $ addExpanded partial

makeLines :: [String] -> Layout -> [String]
makeLines ws layout = let
    maxLength = maximum (map length ws) + 2
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

----------------
-- Word breaking

-- | Break a word at a given set of spaces.  Does not break at a space if it is immediately preceded by
-- the escape character.
-- 
-- > simpleWordBreak Nothing " " (reverse "This is a sentence)          == ("ecnetnes"," a si sihT")
-- > simpleWordBreak (Just '\\') " " (reverse "This is a\\ sentence")   == ("ecnetnes \\a"," si sihT")
-- 
simpleWordBreak :: Maybe Char -- ^ An optional escape character
                    -> String -- ^ List of characters which count as whitespace
                    -> WordBreakFunc
simpleWordBreak Nothing ws = break (`elem` ws)
simpleWordBreak (Just e) ws = escapedBreak
    where
        escapedBreak (c:d:cs) | d == e
            = let (xs,ys) = escapedBreak cs in (c:d:xs,ys)
        escapedBreak (c:cs) | not (elem c ws)
            = let (xs,ys) = escapedBreak cs in (c:xs,ys)
        escapedBreak cs = ("",cs)
    
-- Note: quote marks aren't included here; instead they're stripped by
-- quoteCompletion.
-- | A word break function for filenames.
filenameWordBreak :: WordBreakFunc
filenameWordBreak = simpleWordBreak (Just '\\') " \t\n\\`@$><=;|&{("


-------
-- Expansion

-- Auxiliary functions

-- Simple completion.  If full completion, adds a space.
-- | Make a simple completion function.  If the word is fully expanded, add a space at the end.
basicCompletionFunc :: Monad m => (String -> m [String]) -> CompletionFunc m
basicCompletionFunc f s = liftM (addSpaceIfDone . makeExpansion) (f s)

addSpaceIfDone :: Expansion -> Expansion
addSpaceIfDone (FullExpansion s) = FullExpansion (s ++ " ")
addSpaceIfDone e = e


-- | Ignore quotes when running the completion function, but match any inital quotes if the word is fully
-- expanded.
--
quoteCompletion :: Monad m => (Char -> Bool) -> CompletionFunc m -> CompletionFunc m
quoteCompletion isQuote f = \s -> case s of
    (c:cs) | isQuote c -> f cs >>= \expansion -> return $ case expansion of
                            NoExpansion -> NoExpansion
                            FullExpansion e -> FullExpansion (c : e ++ [c])
                            Partial p ss -> Partial (c:p) ss
    cs -> f cs

-- A completion function for file and folder names.
completeFile :: MonadIO m => CompletionFunc m
completeFile  = liftM addSpaceIfDone . quoteCompletion isQuote fileExpansion
    where 
        fileExpansion word = do
            files <- liftIO (findFiles word)
            let expansion = makeExpansion files
            liftIO (handleFolders expansion)
        isQuote c = c == '\"' || c == '\''

-- A completion command for file and folder names.
fileCompletionCmd :: MonadIO m => Key -> Command m InsertMode InsertMode
fileCompletionCmd = completionCmd filenameWordBreak completeFile


--------
-- Helper funcs for file completion

-- Make some changes specific to folders:
-- A FullExpansion of a folder gets '/' added after it, and is Partial (so no quotes or 
-- spaces added after it)
-- the partial results returned for a subfolder get stripped of their directory parts.
handleFolders :: Expansion -> IO Expansion
handleFolders e@(FullExpansion file) = do
    dirExists <- doesDirectoryExist file
    return $ if dirExists 
                then let dir = addTrailingPathSeparator file in Partial dir [file]
                else e
handleFolders (Partial s ss) = return $ Partial s (map takeFileName ss)
handleFolders NoExpansion = return NoExpansion

-- get a list of files in the current directory which are prefixed by this
-- partial path.
findFiles :: FilePath -> IO [FilePath]
findFiles path = do
    let (dir, file) = splitFileName path
    let filterPrefix = filter (\f -> file `isPrefixOf` f
                                        && f /= "." && f /= "..")
    let dirPath = if null dir then "." else dir
    dirExists <- doesDirectoryExist dirPath
    if not dirExists
        then return []
        else fmap ( map (dir ++) . filterPrefix) (getDirectoryContents dirPath)


