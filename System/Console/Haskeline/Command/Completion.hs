module System.Console.Haskeline.Command.Completion(
                            CompletionFunc,
                            Completion,
                            CompletionType(..),
                            completionCmd
                            ) where

import System.Console.Haskeline.Command
import System.Console.Haskeline.Key
import System.Console.Haskeline.Term(Layout(..))
import System.Console.Haskeline.LineState
import System.Console.Haskeline.InputT
import System.Console.Haskeline.Prefs
import System.Console.Haskeline.Completion
import System.Console.Haskeline.Monads

import Data.List(transpose, unfoldr)

fullReplacement :: Completion -> String
fullReplacement c   | isFinished c  = replacement c ++ " "
                    | otherwise     = replacement c

makeCompletion :: Monad m => InsertMode -> InputCmdT m (InsertMode, [Completion])
makeCompletion (IMode xs ys) = do
    f <- asks complete
    (rest,completions) <- liftCmdT (f (withRev graphemesToString xs, graphemesToString ys))
    return (IMode (withRev stringToGraphemes rest) ys,completions)
  where
    withRev f = reverse . f . reverse

-- | Create a 'Command' for word completion.
completionCmd :: Monad m => Key -> KeyCommand (InputCmdT m) InsertMode InsertMode
completionCmd k = k +> askState (\s -> commandM $ do
    prefs <- ask
    (rest,completions) <- makeCompletion s
    return $ case completionType prefs of
        MenuCompletion -> menuCompletion k s
                        $ map (\c -> insertString (fullReplacement c) rest) completions
        ListCompletion -> 
                pagingCompletion prefs s rest completions k)

pagingCompletion :: Monad m => Prefs
                -> InsertMode -> InsertMode -> [Completion] 
                -> Key -> Command (InputCmdT m) InsertMode InsertMode
pagingCompletion _ oldIM _ [] _ = effect $ RingBell oldIM
pagingCompletion _ _ im [newWord] _ 
        = effect $ Change $ insertString (fullReplacement newWord) im
pagingCompletion prefs oldIM im completions k
    | oldIM /= withPartial = effect $ Change withPartial
    | otherwise = commandM $ do
        layout <- ask
        let wordLines = makeLines (map display completions) layout
        let printingCmd = if completionPaging prefs
                        then printPage wordLines moreMessage
                        else printAll wordLines withPartial
        let pageAction = askFirst (completionPromptLimit prefs) (length completions) 
                            withPartial printingCmd
        if listCompletionsImmediately prefs
            then return pageAction
            else return $ effect (RingBell withPartial) >|> 
                        try (k +> pageAction)
  where
    withPartial = insertString partial im
    partial = foldl1 commonPrefix (map replacement completions)
    commonPrefix (c:cs) (d:ds) | c == d = c : commonPrefix cs ds
    commonPrefix _ _ = ""
    moreMessage = Message withPartial "----More----"

askFirst :: Monad m => Maybe Int -> Int -> InsertMode
            -> Command (InputCmdT m) InsertMode InsertMode
            -> Command (InputCmdT m) InsertMode InsertMode
askFirst mlimit numCompletions im printingCmd = case mlimit of
    Just limit | limit < numCompletions -> 
        effect (Change (Message im ("Display all " ++ show numCompletions
                            ++ " possibilities? (y or n)")))
                    >|> keyChoiceCmd [
                            simpleChar 'y' +> change messageState >|> printingCmd
                            , simpleChar 'n' +> change messageState
                            ]
    _ -> printingCmd

printOneLine :: Monad m => [String] -> Message InsertMode
                    -> Command (InputCmdT m) s InsertMode
printOneLine (w:ws) im | not (null ws) =
            effect (PrintLines [w] im) >|> pagingCommands ws
printOneLine _ im = effect (Change (messageState im))

printPage :: Monad m => [String] -> Message InsertMode
                    -> Command (InputCmdT m) s InsertMode
printPage ws im = commandM $ do
    layout <- ask
    return $ case splitAt (height layout - 1) ws of
        (_,[]) -> effect (PrintLines ws (messageState im))
        (zs,rest) -> effect (PrintLines zs im)
                    >|> pagingCommands rest

-- TODO: move testing of nullity into here
pagingCommands :: Monad m => [String] -> Command (InputCmdT m) (Message InsertMode) InsertMode
pagingCommands ws = keyChoiceCmd [
                            simpleChar ' ' +> askState (printPage ws)
                            ,simpleChar 'q' +> change messageState
                            ,simpleChar '\n' +> askState (printOneLine ws)
                            ,simpleKey DownKey +> askState (printOneLine ws)
                            ]


printAll :: Monad m => [String] -> InsertMode 
            -> Command (InputCmdT m) InsertMode InsertMode
printAll ws im = effect $ PrintLines ws im


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

menuCompletion :: forall m . Monad m => Key -> InsertMode -> [InsertMode] 
                    -> Command m InsertMode InsertMode
menuCompletion _ oldState [] = effect (RingBell oldState)
menuCompletion _ _ [c] = effect (Change c)
menuCompletion k oldState (c:cs) = effect (Change c) >|> loop cs
    where
        loop [] = try $ k +> change (const oldState)
        loop (d:ds) = try $ k +> change (const d) >|> loop ds

