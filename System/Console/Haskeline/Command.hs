module System.Console.Haskeline.Command(
                        -- * Commands
                        Effect(..),
                        KeyMap(..), 
                        CmdStream(..),
                        Command(),
                        KeyCommand(),
                        KeyConsumed(..),
                        withoutConsuming,
                        runCommand,
                        keyCommand,
                        continue,
                        (>|>),
                        (>+>),
                        loopUntil,
                        try,
                        finish,
                        failCmd,
                        effect,
                        askState,
                        commandM,
                        simpleCommand,
                        charCommand,
                        change,
                        changeFromChar,
                        clearScreenCmd,
                        (+>),
                        useChar,
                        choiceCmd,
                        keyChoiceCmd,
                        doBefore,
                        ) where

import Data.Char(isPrint)
import Control.Monad(mplus)
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Key


data Effect s = Change {effectState :: s} 
              | PrintLines {linesToPrint :: [String], effectState :: s}
              | Redraw {shouldClearScreen :: Bool, effectState :: s}
              | RingBell {effectState :: s}

data KeyMap m s = KeyMap {lookupKM :: Key -> Maybe (KeyConsumed (CmdStream m s))}

data KeyConsumed a = NotConsumed a | Consumed a

data CmdStream m s = GetKey (KeyMap m s)
                   | AskState (s -> CmdStream m s)
                   | forall t . LineState t => PutState (Effect t) (CmdStream m t)
                   | StreamM (m (CmdStream m s))
                   | Finish (Maybe String)

newtype KeyCommand m s t = KeyCommand (CmdStream m t -> KeyMap m s)

newtype Command m s t = Command (CmdStream m t -> CmdStream m s)

keyCommand :: KeyCommand m s t -> Command m s t
keyCommand (KeyCommand f) = Command (GetKey . f)

continue :: Command m s s
continue = Command id

useKey :: Key -> KeyCommand m s s
useKey k = KeyCommand $ \next -> KeyMap $ \k' -> if k==k' then Just (Consumed next) else Nothing

useChar :: (Char -> Command m s t) -> KeyCommand m s t
useChar act = KeyCommand $ \next -> KeyMap $ \k -> case k of
                            Key m (KeyChar c) | isPrint c && m==noModifier -> Just $ Consumed $
                                case act c of
                                    Command cact -> cact next
                            _ -> Nothing

withoutConsuming :: Command m s t -> KeyCommand m s t
withoutConsuming (Command f) = KeyCommand $ \next ->
            KeyMap $ const $ Just $ NotConsumed $ f next

choiceCmd :: [KeyCommand m s t] -> KeyCommand m s t
choiceCmd cmds = KeyCommand $ \next -> choiceKM $ map (\(KeyCommand f) -> f next) cmds
    where
        choiceKM = foldl orKM nullKM
        nullKM = KeyMap $ const Nothing
        orKM (KeyMap f) (KeyMap g) = KeyMap $ \k -> f k `mplus` g k

keyChoiceCmd :: [KeyCommand m s t] -> Command m s t
keyChoiceCmd = keyCommand . choiceCmd

runCommand :: KeyCommand m s s -> KeyMap m s
runCommand (KeyCommand f) = let m = f (GetKey m) in m

infixr 6 >|>
(>|>) :: Command m s t -> Command m t u -> Command m s u
Command f >|> Command g = Command (f . g)

infixr 6 >+>
(>+>) :: KeyCommand m s t -> Command m t u -> KeyCommand m s u
KeyCommand f >+> Command g = KeyCommand $ f . g

loopUntil :: KeyCommand m s s -> KeyCommand m s t -> KeyCommand m s t
loopUntil f g = choiceCmd [g, f >+> keyCommand (loopUntil f g)]

-- attempt to run the command (predicated on getting a valid key); but if it fails, just keep
-- going.
try :: KeyCommand m s s -> Command m s s
try f = keyChoiceCmd [f,withoutConsuming continue]

infixr 6 +>
(+>) :: Key -> Command m s t -> KeyCommand m s t
k +> f = useKey k >+> f

finish :: forall s m t . (Result s, Monad m) => Command m s t
finish = Command $ \_-> AskState $ \s -> Finish $ Just $ toResult s

failCmd :: forall s m t . (LineState s, Monad m) => Command m s t
failCmd = Command $ \_-> Finish Nothing

askState :: (s -> Command m s t) -> Command m s t
askState f = Command $ \next -> AskState $ \s -> case f s of
                Command act -> act next

commandM :: Monad m => m (Command m s t) -> Command m s t
commandM f = Command $ \next -> StreamM $ do
                Command cmd <- f
                return (cmd next)

simpleCommand :: (LineState t, Monad m) => (s -> m (Effect t)) 
                    -> Command m s t
simpleCommand f = Command $ \next -> AskState $ \s -> StreamM $ do
                        t <- f s
                        return $ PutState t next

charCommand :: (LineState t, Monad m) => (Char -> s -> m (Effect t))
                    -> KeyCommand m s t
charCommand f = useChar $ \c -> simpleCommand (f c)

effect :: LineState t => Effect t -> Command m s t
effect t = Command $ \next -> PutState t next

change :: (LineState t, Monad m) => (s -> t) -> Command m s t
change f = askState $ effect . Change . f

changeFromChar :: (LineState t, Monad m) => (Char -> s -> t) -> KeyCommand m s t
changeFromChar f = charCommand (\c s -> return $ Change (f c s))

clearScreenCmd :: LineState s => Command m s s
clearScreenCmd = askState $ effect . Redraw True

doBefore :: Command m s t -> KeyCommand m t u -> KeyCommand m s u
doBefore (Command cmd) (KeyCommand kcmd) = KeyCommand $ \next -> case kcmd next of
        KeyMap km -> KeyMap $ \k -> case km k of
                    Nothing -> Nothing
                    Just (NotConsumed f) -> Just $ NotConsumed $ cmd f
                    Just (Consumed f) -> Just $ Consumed $ cmd f
         
