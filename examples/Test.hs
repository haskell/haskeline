module Main where

import System.Console.Haskeline
import System.Console.Haskeline.Monads

mySettings :: MonadIO m => Settings m
mySettings = defaultSettings {historyFile = Just "myhist",
                        handleSigINT = True}

myComplete :: Monad m => CompletionFunc m
myComplete s = return (s,[])

main :: IO ()
main = runInputT mySettings (loop 0)
    where
        loop :: Int -> InputT IO ()
        loop n = do
            minput <-  handleInterrupt (return (Just "Caught interrupted"))
                        (getInputLine (show n ++ ":"))
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just s -> do
                            putOutputStrLn ("line " ++ show n ++ ":" ++ s)
                            loop (n+1)
