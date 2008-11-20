module Main where

import System.Console.Haskeline

mySettings :: Settings IO
mySettings = defaultSettings {historyFile = Just "myhist"}

main :: IO ()
main = runInputT mySettings $ withInterrupt $ loop 0
    where
        loop :: Int -> InputT IO ()
        loop n = do
            minput <-  handleInterrupt (return (Just "Caught interrupted"))
                        $ getInputLine (show n ++ ":")
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just s -> do
                            outputStrLn ("line " ++ show n ++ ":" ++ s)
                            loop (n+1)
