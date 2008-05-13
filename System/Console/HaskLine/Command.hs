module System.Console.HaskLine.Command(
                        Key(..),
                        controlKey,
                        -- * Commands
                        Effect(..),
                        Layout(..),
                        KeyMap(), 
                        lookupKM,
                        KeyAction(..),
                        Command(),
                        runCommand,
                        continue,
                        (>|>),
                        (>+>),
                        acceptKey,
                        acceptKeyM,
                        acceptChar,
                        loopUntil,
                        loopWithBreak,
                        try,
                        finish,
                        simpleCommand,
                        change,
                        changeWithoutKey,
                        clearScreenCmd,
                        (+>),
                        choiceCmd,
                        spliceCmd
                        ) where

import qualified Data.Map as Map
import Data.Bits
import System.Console.HaskLine.LineState
import System.Console.HaskLine.Draw
import System.Console.Terminfo
import System.Console.HaskLine.Posix


-- Easy translation of control characters; e.g., Ctrl-G or Ctrl-g or ^G
controlKey :: Char -> Key
controlKey '?' = KeyChar (toEnum 127)
controlKey c = KeyChar $ toEnum $ fromEnum c .&. complement (bit 5 .|. bit 6)


data Effect s = Change s | PrintLines (Layout -> [String]) s
                | Redraw {shouldClearScreen :: Bool, redrawState :: s}
                | Fail | Finish

newtype KeyMap m s = KeyMap {keyMap :: Map.Map Key (s -> m (KeyAction m))}

data KeyAction m = forall t . LineState t => KeyAction (Effect t) (KeyMap m t)

lookupKM :: KeyMap m s -> Key -> Maybe (s -> m (KeyAction m))
lookupKM km k = Map.lookup k (keyMap km)

nullKM :: KeyMap m s
nullKM = KeyMap $ Map.empty

orKM :: KeyMap m s -> KeyMap m s -> KeyMap m s
orKM (KeyMap m) (KeyMap n) = KeyMap $ m `Map.union` n

choiceKM :: [KeyMap m s] -> KeyMap m s
choiceKM = foldl orKM nullKM

newtype Command m s t = Command (KeyMap m t -> KeyMap m s)

runCommand :: Command m s s -> KeyMap m s
runCommand (Command f) = let m = f m in m

continue :: Command m s s
continue = Command id

infixl 6 >|>
(>|>) :: Command m s t -> Command m t u -> Command m s u
Command f >|> Command g = Command (f . g)

infixl 6 >+>
(>+>) :: (Monad m, LineState s) => Key -> Command m s t -> Command m s t
k >+> f = k +> change id >|> f

acceptKey :: (Monad m, LineState t) => 
                Key -> (s -> m (Effect t)) -> Command m s t
acceptKey k act = Command $ \next -> KeyMap $ Map.singleton k $ \s -> do 
                                                t <- act s
                                                return (KeyAction t next)

acceptChar :: (Monad m, LineState t) => (Char -> s -> t) -> Command m s t
acceptChar f = Command $ \next -> KeyMap $ Map.fromList 
                $ map (\c -> (KeyChar c,\s -> return (KeyAction (Change (f c s)) next)))
                    [' '..'~']

acceptKeyM :: (Monad m, LineState s) => Key -> (s -> m (Effect s, Command m s s))
                            -> Command m s s
acceptKeyM k f = Command $ \next -> KeyMap $ Map.singleton k $ \s -> do
                (effect,Command g) <- f s
                return (KeyAction effect (g next))
                         
loopUntil :: Command m s s -> Command m s t -> Command m s t
loopUntil (Command f) (Command g) 
    = Command $ \next -> let loop = g next `orKM` f loop
                         in loop

loopWithBreak :: Command m s s -> Command m s t -> (s -> t) -> Command m s t
loopWithBreak cmd end f = Command $ \next -> 
    runCommand $ choiceCmd [cmd, spliceCmd next end, 
                        spliceCmd next (changeWithoutKey f)]

try :: Command m s s -> Command m s s
try (Command f) = Command $ \next -> (f next) `orKM` next

finish :: forall s m t . (LineState t, Monad m) => Key -> Command m s t
finish k = acceptKey k $ const (return Finish)

-- NOTE: SAME AS ACCEPTKEY
simpleCommand :: (LineState t, Monad m) => (s -> m (Effect t)) 
                    -> Key -> Command m s t
simpleCommand f k = acceptKey k f

change :: (LineState t, Monad m) => (s -> t) -> Key -> Command m s t
change f = simpleCommand (return . Change . f)

changeWithoutKey :: (s -> t) -> Command m s t
changeWithoutKey f = Command $ \(KeyMap next) -> KeyMap $ fmap (\g -> g . f) next
    -- (\(KeyAction g next') -> KeyAction (g . f) next') next

clearScreenCmd :: (LineState s, Monad m) => Key -> Command m s s
clearScreenCmd k = acceptKey k $ \s -> return (Redraw True s)

infixl 7 +>
(+>) :: Key -> (Key -> a) -> a 
k +> f = f k

choiceCmd :: [Command m s t] -> Command m s t
choiceCmd cmds = Command $ \next -> 
    choiceKM $ map (\(Command f) -> f next) cmds

spliceCmd :: KeyMap m t -> Command m s t -> Command m s u
spliceCmd alternate (Command f) = Command $ \_-> f alternate


