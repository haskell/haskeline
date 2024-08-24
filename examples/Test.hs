module Main where

import Data.List
import System.Console.Haskeline
import System.Environment

{--
Testing the line-input functions and their interaction with ctrl-c signals.

Usage:
./Test          (line input)
./Test chars    (character input)
./Test password (no masking characters)
./Test password \*
./Test initial  (use initial text in the prompt)
./Test description (completion with descriptions)
--}

mySettings :: Settings IO
mySettings = defaultSettings {historyFile = Just "myhist"}

completeWithDesc :: CompletionFunc IO
completeWithDesc (l, r) = return ([], completions)
  where
    items = [ "first"
            , "second"
            , "third"
            , "forth"
            , "fifth"
            ]
    filterFunc d = (reverse l) `isPrefixOf` d && r `isSuffixOf` d
    filtered = filter filterFunc items
    replacements = (\x -> fst $ splitAt (length x - length r) x) <$> filtered
    descriptions = map (\x -> Just $ "this is the " <> x <> " item") filtered
    finished = replicate (length filtered) (null r)
    completions = zipWith4 Completion replacements filtered descriptions finished

main :: IO ()
main = do
        args <- getArgs
        let inputFunc = case args of
                ["chars"] -> fmap (fmap (\c -> [c])) . getInputChar
                ["password"] -> getPassword Nothing
                ["password", [c]] -> getPassword (Just c)
                ["initial"] -> flip getInputLineWithInitial ("left ", "right")
                _ -> getInputLine
            settings = case args of
                ["description"] -> setComplete completeWithDesc mySettings
                _ -> mySettings
        runInputT settings $ withInterrupt $ loop inputFunc 0
    where
        loop :: (String -> InputT IO (Maybe String)) -> Int -> InputT IO ()
        loop inputFunc n = do
            minput <-  handleInterrupt (return (Just "Caught interrupted"))
                        $ inputFunc (show n ++ ":")
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just "q" -> return ()
                Just s -> do
                            outputStrLn ("line " ++ show n ++ ":" ++ s)
                            loop inputFunc (n+1)
