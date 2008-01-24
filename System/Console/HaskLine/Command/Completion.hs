module System.Console.HaskLine.Command.Completion(
                            CompletionFunc,
                            Completion,
                            WordBreakFunc,
                            simpleWordBreak,
                            completionCmd,
                            menuCompleteCmd,
                            -- * Filename completion
                            filenameWordBreak,
                            fileCompletionCmd,
                            quotedFilenames,
                            -- * Helper functions
                            simpleCompletionFunc,
                            ) where

import System.Console.HaskLine.Command
import System.Console.HaskLine.Modes
import System.Console.HaskLine.LineState

import System.Directory
import System.FilePath
import Control.Monad(liftM)
import Control.Monad.Trans
import Data.List(isPrefixOf, transpose, unfoldr)

-- | Break off a reversed word from a reversed string.  The input and output 'String's are reversed.
-- 
-- For example, @break (==' ')@ is a 'WordBreakFunc':
--
-- >  break (==' ') (reverse "This is a sentence") == ("ecnetnes"," a si sihT")
-- 
type WordBreakFunc = String -> (String, String)

type MakeCompletion m = InsertMode -> m (InsertMode,[Completion])
makeCompletion :: Monad m => WordBreakFunc -> CompletionFunc m 
                    -> MakeCompletion m
makeCompletion breakWord complete (IMode xs ys) = do
    let (revWord,rest) = breakWord xs
    let word = reverse revWord
    completions <- complete word
    return (IMode rest ys,completions)

-- | Create a 'Command' for word completion.
completionCmd :: Monad m => WordBreakFunc -> CompletionFunc m 
    -> Key -> Command m InsertMode InsertMode
completionCmd breakWord complete = simpleCommand $ \im -> do
    (im',completions) <- makeCompletion breakWord complete im
    return $ case completions of
        [] -> Change im
        [newWord] -> Change $ insertString (replacement newWord) im'
        _
            | im /= withPartial -> Change withPartial
            | otherwise -> PrintLines (makeLines $ map display completions)
                            withPartial
          where
            partial = foldl1 commonPrefix (map replacement completions)
            withPartial = insertString partial im'
            commonPrefix (c:cs) (d:ds) | c == d = c : commonPrefix cs ds
            commonPrefix _ _ = ""

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


data DoCompletions = PrefixCompletion {
                        allCompletions :: [String],
                        thisCompletion :: String,
                        remainingCompletions :: [String],
                        completionState :: InsertMode
                       }
                      | FullCompletion {completionState :: InsertMode}

completionFullState :: DoCompletions -> InsertMode
completionFullState (FullCompletion im) = im
completionFullState PrefixCompletion {thisCompletion = this,
                        completionState = s}
                    = insertString this s

instance LineState DoCompletions where
    beforeCursor prefix = beforeCursor prefix . completionFullState
    afterCursor = afterCursor . completionFullState
    toResult = toResult . completionFullState

continueCompletion :: Monad m => MakeCompletion m 
                        -> DoCompletions -> m (Effect DoCompletions)
continueCompletion f fc@(FullCompletion im) = startCompletion f im
continueCompletion f p@PrefixCompletion {} = return $ Change $ 
    case remainingCompletions p of
        [] -> p {thisCompletion = head (allCompletions p), 
                    remainingCompletions = tail (allCompletions p)}
        (w:ws) -> p{thisCompletion=w,remainingCompletions=ws}

startCompletion :: Monad m => MakeCompletion m -> InsertMode
                        -> m (Effect DoCompletions)
startCompletion f im = do
    (im',completions) <- f im
    return $ Change $ case map replacement completions of
        [] -> FullCompletion  im
        [w] -> FullCompletion $ insertString w im'
        ws -> PrefixCompletion {
                            allCompletions = ws,
                            thisCompletion = head ws,
                            remainingCompletions = tail ws,
                            completionState = im'
                            }


menuCompleteCmd :: Monad m => WordBreakFunc -> CompletionFunc m
    -> Key -> Command m InsertMode InsertMode
menuCompleteCmd breakWord complete k = let
    mkComplete = makeCompletion breakWord complete
    continueComplete = loopWithBreak 
                            (k +> simpleCommand (continueCompletion mkComplete))
                            (choiceCmd [])
                            completionFullState
    in k +> simpleCommand (startCompletion mkComplete) >|> continueComplete

--
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

-- A completion command for file and folder names.
fileCompletionCmd :: MonadIO m => Key -> Command m InsertMode InsertMode
fileCompletionCmd = completionCmd
                    -- menuCompleteCmd 
                    filenameWordBreak (liftIO . quotedFilenames isQuote)
    where
        isQuote c = c == '\"' || c == '\''


data Completion = Completion {replacement, display :: String}
                    deriving Show

completion :: String -> Completion
completion str = Completion str str

setReplacement, setDisplay :: (String -> String) -> Completion -> Completion
setReplacement f c = c {replacement = f $ replacement c}
setDisplay f c = c {display = f $ display c}


type CompletionFunc m = String -> m [Completion]

-- | Puts a space after word when completed.
simpleCompletionFunc :: MonadIO m => (String -> IO [String]) 
        -> String -> m [Completion]
simpleCompletionFunc f = liftIO 
                            . fmap (map (setReplacement (++ " ") . completion)) 
                            . f

--------
-- Helper funcs for file completion

quotedFilenames :: (Char -> Bool) -> String -> IO [Completion]
quotedFilenames isQuote (q:file) | isQuote q = do
    files <- findFiles file
    return $ map (setReplacement ((q:) . appendIfNotDir [q,' '])) files
quotedFilenames _ file = do
    files <- findFiles file
    return $ map (setReplacement (appendIfNotDir " ")) files

appendIfNotDir str file | null (takeFileName file) = file
                        | otherwise = file ++ str

-- get a list of files in the current directory which are prefixed by this
-- partial path.
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

addSlashToDir :: Completion -> IO Completion
addSlashToDir c@Completion{replacement=file} = do
    dirExists <- doesDirectoryExist file
    return $ if dirExists 
                then c {replacement = addTrailingPathSeparator file}
                else c

