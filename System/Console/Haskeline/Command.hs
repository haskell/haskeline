module System.Console.Haskeline.Command(
                        -- * Commands
                        Effect(..),
                        KeyMap(), 
                        lookupKM,
                        KeyAction(..),
                        CmdAction(..),
                        (>=>),
                        Command(),
                        runCommand,
                        continue,
                        (>|>),
                        (>+>),
                        acceptKey,
                        acceptKeyM,
                        acceptKeyOrFail,
                        loopUntil,
                        try,
                        finish,
                        failCmd,
                        simpleCommand,
                        charCommand,
                        change,
                        changeFromChar,
                        changeWithoutKey,
                        clearScreenCmd,
                        (+>),
                        choiceCmd,
                        withState
                        ) where

import Data.Char(isPrint)
import Control.Monad(mplus)
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Key


data Effect s = Change {effectState :: s} 
              | PrintLines {linesToPrint :: [String], effectState :: s}
              | Redraw {shouldClearScreen :: Bool, effectState :: s}
              | RingBell {effectState :: s}

newtype KeyMap m s = KeyMap {lookupKM :: Key -> Maybe 
            (s -> Either (Maybe String) (m (KeyAction m)))}

useKey :: Key -> (s -> Either (Maybe String) (m (KeyAction m))) -> KeyMap m s
useKey k f = KeyMap $ \k' -> if k==k' then Just f else Nothing

data KeyAction m = forall t . LineState t => KeyAction (Effect t) (KeyMap m t)

nullKM :: KeyMap m s
nullKM = KeyMap $ const Nothing

orKM :: KeyMap m s -> KeyMap m s -> KeyMap m s
orKM (KeyMap m) (KeyMap n) = KeyMap $ \k -> m k `mplus` n k

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

data CmdAction m s = forall t . LineState t => CmdAction (Effect t) (Command m t s)

(>=>) :: LineState t => Effect t -> Command m t s -> CmdAction m s
(>=>) = CmdAction

acceptKey :: (Monad m) => (s -> CmdAction m t) -> Key -> Command m s t
acceptKey f = acceptKeyFull (Just . return . f)

acceptKeyM :: Monad m => (s -> m (CmdAction m t)) -> Key -> Command m s t
acceptKeyM f = acceptKeyFull (Just . f)

acceptKeyFull :: Monad m => (s -> Maybe (m (CmdAction m t)))
                            -> Key -> Command m s t
acceptKeyFull f k = Command $ \next -> useKey k $ \s -> case f s of
                Nothing -> Left Nothing
                Just act -> Right $ do
                    CmdAction effect (Command g) <- act
                    return (KeyAction effect (g next))

acceptKeyOrFail :: Monad m => (s -> Maybe (CmdAction m t)) -> Key
            -> Command m s t
acceptKeyOrFail f = acceptKeyFull (fmap return . f)
                         
loopUntil :: Command m s s -> Command m s t -> Command m s t
loopUntil f g = choiceCmd [g, f >|> loopUntil f g]

try :: Command m s s -> Command m s s
try (Command f) = Command $ \next -> (f next) `orKM` next

finish :: forall s m t . (Result s, Monad m) => Key -> Command m s t
finish k = Command $ \_-> useKey k (Left . Just . toResult)

failCmd :: forall s m t . (LineState s, Monad m) => Key -> Command m s t
failCmd k = Command $ \_-> useKey k (const $ Left Nothing)

simpleCommand :: (LineState t, Monad m) => (s -> m (Effect t)) 
                    -> Key -> Command m s t
simpleCommand f = acceptKeyM $ \s -> do
            act <- f s
            return (act >=> continue)

charCommand :: (LineState t, Monad m) => (Char -> s -> m (Effect t))
                    -> Command m s t
charCommand f = Command $ \next -> KeyMap $ \k -> case k of
                    Key Nothing (KeyChar c) | isPrint c -> Just $ \s -> Right $ do
                                    effect <- f c s
                                    return (KeyAction effect next)
                    _ -> Nothing
                    

change :: (LineState t, Monad m) => (s -> t) -> Key -> Command m s t
change f = simpleCommand (return . Change . f)

changeFromChar :: (Monad m, LineState t) => (Char -> s -> t) -> Command m s t
changeFromChar f = charCommand (\c s -> return $ Change (f c s))

changeWithoutKey :: (s -> t) -> Command m s t
changeWithoutKey f = Command $ \(KeyMap next) -> KeyMap $ fmap (. f) . next

clearScreenCmd :: (LineState s, Monad m) => Key -> Command m s s
clearScreenCmd k = k +> simpleCommand (\s -> return (Redraw True s))

infixl 7 +>
(+>) :: Key -> (Key -> a) -> a 
k +> f = f k

choiceCmd :: [Command m s t] -> Command m s t
choiceCmd cmds = Command $ \next -> 
    choiceKM $ map (\(Command f) -> f next) cmds

withState :: Monad m => (s -> m a) -> Command m s t -> Command m s t
withState act (Command thisCmd) = Command $ \next -> KeyMap $ \k -> 
    case lookupKM (thisCmd next) k of
        Nothing -> Nothing
        Just f -> Just $ \s -> case f s of
            Left r -> Left r
            Right effect -> Right $ act s >> effect
