module Command where

import LineState
import System.Console.Terminfo
import Data.Maybe

import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import System.Console.Terminfo
import System.Posix (Fd,stdOutput)
import System.Posix.Terminal

data Key = KeyChar Char | KeySpecial SKey
                deriving (Show,Eq,Ord)
data SKey = KeyLeft | KeyRight | KeyUp | KeyDown
            | Backspace | DeleteForward
                deriving (Eq,Ord,Enum,Show)

getKeySequences :: Terminal -> IO (Map.Map String SKey)
getKeySequences term = do
    sttys <- sttyKeys
    return $ Map.union sttys (terminfoKeys term)

terminfoKeys :: Terminal -> Map.Map String SKey
terminfoKeys term = Map.fromList $ catMaybes 
                    $ map getSequence keyCapabilities
        where getSequence (cap,x) = getCapability term $ do 
                            keys <- cap
                            return (keys,x)

keyCapabilities = [(keyLeft,KeyLeft),
                (keyRight,KeyRight),
                (keyUp,KeyUp),
                (keyDown,KeyDown),
                (keyBackspace,Backspace),
                (keyDeleteChar,DeleteForward)]

sttyKeys :: IO (Map.Map String SKey)
sttyKeys = do
    attrs <- getTerminalAttributes stdOutput
    return $ case controlChar attrs Erase of
        Just c -> Map.singleton [c] Backspace
        Nothing -> Map.empty

getKey :: [(String, SKey)] -> IO Key
getKey ms = do 
    c <- getChar
    case mapMaybe (matchHead c) ms of
        [] -> return (KeyChar c)
        [("",k)] -> return (KeySpecial k)
        ms' -> getKey ms'
  where
    matchHead c (d:ds,k)  | c == d = Just (ds,k)
    matchHead _ _                  = Nothing


        
{-- todo: some commands only change the linestate, don't require
 a full refresh:
 data Command m = Change (LSCHANGE) | Refresh (Linestate -> m LineState)
 --}
data Command (m :: * -> *) = Finish | ChangeCmd LineChange

type Commands m = Map.Map Key (Command m)

simpleCommands :: Monad m => Commands m
simpleCommands = Map.fromList $ [
                    (KeyChar '\n', Finish)
                    ,(KeySpecial KeyLeft, ChangeCmd goLeft)
                    ,(KeySpecial KeyRight, ChangeCmd goRight)
                    ,(KeySpecial Backspace, ChangeCmd deletePrev)
                    ,(KeySpecial DeleteForward, ChangeCmd deleteNext)
                    ] ++ map insertionCommand [' '..'~']
            

insertionCommand :: Monad m => Char -> (Key,Command m)
insertionCommand c = (KeyChar c, ChangeCmd $ insertChar c)

                    
