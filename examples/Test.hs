module Main where

import System.Console.Haskeline
import System.Console.Haskeline.Monads
import qualified System.IO.UTF8 as UTF8

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
                            liftIO $ UTF8.putStrLn ("line " ++ show n ++ ":" ++ s)
                            loop (n+1)
