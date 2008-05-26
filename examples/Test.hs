module Main where

import System.Console.Haskeline
import System.Console.Haskeline.Monads

main :: IO ()
main = runInputT defaultSettings {historyFile = Just "myhist"}
                    (loop 0)
    where
        loop :: Int -> InputT IO ()
        loop n = do
            minput <- getInputLine (show n ++ ":")
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just s -> do
                            liftIO $ putStrLn ("line " ++ show n ++ ":" ++ s)
                            loop (n+1)
