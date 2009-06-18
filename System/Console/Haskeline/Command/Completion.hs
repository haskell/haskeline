module System.Console.Haskeline.Command.Completion(
                            CompletionFunc,
                            Completion,
                            CompletionType(..),
                            completionCmd
                            ) where

import System.Console.Haskeline.Command
import System.Console.Haskeline.Command.Undo
import System.Console.Haskeline.Key
import System.Console.Haskeline.Term(Layout(..))
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Prefs
import System.Console.Haskeline.Completion
import System.Console.Haskeline.Monads

import Data.List(transpose, unfoldr)

useCompletion :: InsertMode -> Completion -> InsertMode
useCompletion im c = insertString r im
    where r | isFinished c = replacement c ++ " "
            | otherwise = replacement c

askIMCompletions :: (InsertMode -> [Completion] -> Command m InsertMode t) -> Command m InsertMode t
askIMCompletions act = askState $ \(IMode xs ys) -> 
    askCompletions (withRev graphemesToString xs, graphemesToString ys) $ \rest completions -> 
        act (IMode (withRev stringToGraphemes rest) ys) completions
  where
    withRev f = reverse . f . reverse

-- | Create a 'Command' for word completion.
completionCmd :: (MonadState Undo m, MonadReader Prefs m, MonadReader Layout m)
                => Key -> KeyCommand m InsertMode InsertMode
completionCmd k = k +> saveForUndo >|> askIMCompletions (\rest cs
    -> case cs of
        [] -> effect RingBell
        [c] -> putState $ useCompletion rest c
        _ -> presentCompletions k rest cs)

presentCompletions :: (MonadReader Prefs m, MonadReader Layout m) => Key -> InsertMode
            -> [Completion] -> Command m InsertMode InsertMode
presentCompletions k rest cs = askState $ \oldIM -> commandM $ do
    prefs <- ask
    return $ case completionType prefs of
        MenuCompletion -> menuCompletion k oldIM (map (useCompletion rest) cs)
        ListCompletion -> let withPartial = makePartialCompletion rest cs
                          in putState withPartial
                                >|> if withPartial /= oldIM
                                        then continue
                                        else pagingCompletion k prefs cs

menuCompletion :: Key -> InsertMode -> [InsertMode] -> Command m InsertMode InsertMode
menuCompletion k oldIM = loop
    where
        loop [] = putState oldIM
        loop (c:cs) = putState c >|> try (k +> loop cs)

makePartialCompletion :: InsertMode -> [Completion] -> InsertMode
makePartialCompletion im completions = insertString partial im
  where
    partial = foldl1 commonPrefix (map replacement completions)
    commonPrefix (c:cs) (d:ds) | c == d = c : commonPrefix cs ds
    commonPrefix _ _ = ""

pagingCompletion :: MonadReader Layout m => Key -> Prefs
                -> [Completion] -> Command m InsertMode InsertMode
pagingCompletion k prefs completions = commandM $ do
        ls <- asks $ makeLines (map display completions)
        let pageAction = askFirst prefs (length completions) $ 
                            if completionPaging prefs
                                then change moreMessage >|> printPage ls
                                else effect (PrintLines ls)
        return $ if listCompletionsImmediately prefs
            then pageAction
            else effect RingBell >|> try (k +> pageAction)
  where
    moreMessage = flip Message "----More----"

-- TODO: test that all prefs still work OK.

askFirst :: Prefs -> Int -> Command m InsertMode InsertMode
            -> Command m InsertMode InsertMode
askFirst prefs n cmd
    | maybe False (< n) (completionPromptLimit prefs)
        = change (flip Message $ "Display all " ++ show n
                            ++ " possibilities? (y or n)")
                    >|> keyChoiceCmd [
                            simpleChar 'y' +> change messageState >|> cmd
                            , simpleChar 'n' +> change messageState
                            ]
    | otherwise = cmd

printOneLine :: MonadReader Layout m => [String] -> Command m (Message InsertMode) InsertMode
printOneLine (w:ws) = effect (PrintLines [w]) >|> pageCompletions ws
printOneLine _ = change messageState -- shouldn't happen

printPage :: MonadReader Layout m => [String] -> Command m (Message InsertMode) InsertMode
printPage ws = commandM $ do
    layout <- ask
    let (zs,rest) = splitAt (height layout - 1) ws
    return $ effect (PrintLines zs) >|> pageCompletions rest

pageCompletions :: MonadReader Layout m => [String] -> Command m (Message InsertMode) InsertMode
pageCompletions [] = change messageState
pageCompletions ws = keyChoiceCmd [
                            simpleChar ' ' +> printPage ws
                            ,simpleChar 'q' +> change messageState
                            ,simpleChar '\n' +> printOneLine ws
                            ,simpleKey DownKey +> printOneLine ws
                            ]

-----------------------------------------------
-- Splitting the list of completions into lines for paging.
makeLines :: [String] -> Layout -> [String]
makeLines ws layout = let
    minColPad = 2
    printWidth = width layout
    maxLength = min printWidth (maximum (map length ws) + minColPad)
    numCols = printWidth `div` maxLength
    ls = if maxLength >= printWidth
                    then map (\x -> [x]) ws
                    else splitIntoGroups numCols ws
    in map (padWords maxLength) ls

-- Add spaces to the end of each word so that it takes up the given length.
-- Don't padd the word in the last column, since printing a space in the last column
-- causes a line wrap on some terminals.
padWords :: Int -> [String] -> String
padWords _ [x] = x
padWords _ [] = ""
padWords len (x:xs) = x ++ replicate (len - glength x) ' '
			++ padWords len xs
    where
        -- kludge: compute the length in graphemes, not chars.
        -- but don't use graphemes for the max length, since I'm not convinced
        -- that would work correctly. (This way, the worst that can happen is
        -- that columns are longer than necessary.)
        glength = length . stringToGraphemes

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


