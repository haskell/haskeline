module Main where

import System.Console.Haskeline
import System.Console.Haskeline.Monads

main :: IO ()
main = runHaskLineT defaultSettings (loop 0)
    where
        loop :: Int -> HaskLineT IO ()
        loop n = do
            minput <- getHaskLine (show n ++ ":")
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just s -> do
                            liftIO $ putStrLn ("line " ++ show n ++ ":" ++ s)
                            loop (n+1)
