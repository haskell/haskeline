module System.Console.Haskeline.DumbTerm where

newtype DumbTerm m a = DumbTerm (m a) -- keep track of window
                deriving (Monad,MonadIO)

instance MonadTrans DumbTerm where
    lift = DumbTerm
    lift2 f (DumbTerm m) = DumbTerm (f m)


{--
ansiActions :: Actions
ansiActions = Actions {leftA = numArg "\ESC[D",
                        rightA = numArg "\ESC[C",
                        upA = numArg "\ESC[A",
                        clearToLineEnd = termText "\ESC[K",
                        nl = termText "\n",
                        cr = termText "\r",
                        clearAll = \_ -> termText "\ESC[H\ESC[J",
                        wrapLine = mempty -- Not sure about this...
                        }
    where
        numArg s k = termText $ concat $ replicate k s
--}

