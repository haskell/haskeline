module System.Console.Haskeline.Completion(
                            CompletionFunc,
                            Completion(..),
                            completeWord,
                            simpleCompletion,
                            noCompletion,
                            completeFilename,
                            filenameWordBreakChars
                        ) where


import System.Directory
import System.FilePath
import Data.List(isPrefixOf)
import Control.Monad(forM)

import System.Console.Haskeline.Monads

-- | Performs completions from a reversed 'String'.  
-- The output 'String' is also reversed.
-- Use 'completeWord' to build these functions.

type CompletionFunc m = String -> m (String, [Completion])


data Completion = Completion {replacement  :: String, -- ^ Text to insert in line.
                        display  :: String
                                -- ^ Text to display when listing
                                -- alternatives.
                            }
                    deriving Show

-- | Disable completion altogether.
noCompletion :: Monad m => CompletionFunc m
noCompletion s = return (s,[])

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

