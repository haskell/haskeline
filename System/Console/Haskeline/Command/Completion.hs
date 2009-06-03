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
completionCmd :: Monad m => Key -> Command (InputCmdT m) InsertMode InsertMode
completionCmd k = k +> acceptKeyM (\s -> do
    prefs <- ask
    (rest,completions) <- makeCompletion s
    case completionType prefs of
        MenuCompletion -> return $ menuCompletion k s
                        $ map (\c -> insertString (fullReplacement c) rest) completions
        ListCompletion -> 
                pagingCompletion prefs s rest completions k)

pagingCompletion :: Monad m => Prefs
                -> InsertMode -> InsertMode -> [Completion] 
                -> Key -> InputCmdT m (CmdAction (InputCmdT m) InsertMode)
pagingCompletion _ oldIM _ [] _ = return $ RingBell oldIM >=> continue
pagingCompletion _ _ im [newWord] _ 
        = return $ (Change $ insertString (fullReplacement newWord) im) >=> continue
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
                            simpleChar 'y' +> acceptKey (const printingCmd)
                            , simpleChar 'n' +> change messageState
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
                            simpleChar ' ' +> acceptKeyM (printPage ws)
                            ,simpleChar 'q' +> change messageState
                            ,simpleChar '\n' +> acceptKey (printOneLine ws)
                            ,simpleKey DownKey +> acceptKey (printOneLine ws)
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
                    -> CmdAction m InsertMode
menuCompletion _ oldState [] = RingBell oldState >=> continue
menuCompletion _ _ [c] = Change c >=> continue
menuCompletion k oldState (c:cs) = Change c >=> loop cs
    where
        loop [] = choiceCmd [change (const oldState) k,continue]
        loop (d:ds) = choiceCmd [change (const d) k >|> loop ds,continue]

