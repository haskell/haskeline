module System.Console.Haskeline.LineState where


class LineState s where
    beforeCursor :: String -> s -> String -- text to left of cursor
    afterCursor :: s -> String -- text under and to right of cursor

class LineState s => Result s where
    toResult :: s -> String

lengthToEnd :: LineState s => s -> Int
lengthToEnd = length . afterCursor

class (Result s) => FromString s where
    fromString :: String -> s

class Move s where
    goLeft, goRight, moveToStart, moveToEnd :: s -> s
    

data InsertMode = IMode String String 
                    deriving (Show, Eq)

instance LineState InsertMode where
    beforeCursor prefix (IMode xs _) = prefix ++ reverse xs
    afterCursor (IMode _ ys) = ys

instance Result InsertMode where
    toResult (IMode xs ys) = reverse xs ++ ys

instance Move InsertMode where
    goLeft im@(IMode [] _) = im 
    goLeft (IMode (x:xs) ys) = IMode xs (x:ys)

    goRight im@(IMode _ []) = im
    goRight (IMode ys (x:xs)) = IMode (x:ys) xs

    moveToStart (IMode xs ys) = IMode [] (reverse xs ++ ys)
    moveToEnd (IMode xs ys) = IMode (reverse ys ++ xs) []

instance FromString InsertMode where
    fromString s = IMode (reverse s) []

emptyIM :: InsertMode
emptyIM = IMode "" ""

insertChar :: Char -> InsertMode -> InsertMode
insertChar c (IMode xs ys) = IMode (c:xs) ys

insertString :: String -> InsertMode -> InsertMode
insertString s (IMode xs ys) = IMode (reverse s ++ xs) ys

deleteNext, deletePrev :: InsertMode -> InsertMode
deleteNext im@(IMode _ []) = im
deleteNext (IMode xs (_:ys)) = IMode xs ys

deletePrev im@(IMode [] _) = im
deletePrev (IMode (_:xs) ys) = IMode xs ys 

skipLeft, skipRight :: (Char -> Bool) -> InsertMode -> InsertMode
skipLeft f (IMode xs ys) = let (ws,zs) = span f xs 
                           in IMode zs (reverse ws ++ ys)
skipRight f (IMode xs ys) = let (ws,zs) = span f ys 
                            in IMode (reverse ws ++ xs) zs


data CommandMode = CMode String Char String | CEmpty
                    deriving Show

instance LineState CommandMode where
    beforeCursor prefix CEmpty = prefix
    beforeCursor prefix (CMode xs _ _) = prefix ++ reverse xs
    afterCursor CEmpty = ""
    afterCursor (CMode _ c ys) = c:ys

instance Result CommandMode where
    toResult CEmpty = ""
    toResult (CMode xs c ys) = reverse xs ++ (c:ys)

instance Move CommandMode where
    goLeft (CMode (x:xs) c ys) = CMode xs x (c:ys)
    goLeft cm = cm

    goRight (CMode xs c (y:ys)) = CMode (c:xs) y ys
    goRight cm = cm

    moveToStart (CMode xs c ys) = let zs = reverse xs ++ (c:ys) in CMode [] (head zs) (tail zs)
    moveToStart CEmpty = CEmpty

    moveToEnd (CMode xs c ys) = let zs = reverse ys ++ (c:xs) in CMode (tail zs) (head zs) []
    moveToEnd CEmpty = CEmpty

instance FromString CommandMode where
    fromString s = case reverse s of
                    [] -> CEmpty
                    (c:cs) -> CMode cs c []

deleteChar :: CommandMode -> CommandMode
deleteChar (CMode xs _ (y:ys)) = CMode xs y ys
deleteChar (CMode (x:xs) _ []) = CMode xs x []
deleteChar _ = CEmpty

replaceChar :: Char -> CommandMode -> CommandMode
replaceChar c (CMode xs _ ys) = CMode xs c ys
replaceChar _ CEmpty = CEmpty



------------------------
-- Transitioning between modes

enterCommandMode :: InsertMode -> CommandMode
enterCommandMode (IMode (x:xs) ys) = CMode xs x ys
enterCommandMode (IMode [] (y:ys)) = CMode [] y ys
enterCommandMode _ = CEmpty

insertFromCommandMode, appendFromCommandMode :: CommandMode -> InsertMode

insertFromCommandMode CEmpty = emptyIM
insertFromCommandMode (CMode xs c ys) = IMode xs (c:ys)

appendFromCommandMode CEmpty = emptyIM
appendFromCommandMode (CMode xs c ys) = IMode (c:xs) ys


----------------------
-- Supplementary modes

data ArgMode s = ArgMode {arg :: Int, argState :: s}

instance LineState s => LineState (ArgMode s) where
    beforeCursor _ am = beforeCursor ("(arg: " ++ show (arg am) ++ ") ")
                            (argState am)
    afterCursor = afterCursor . argState

instance Result s => Result (ArgMode s) where
    toResult = toResult . argState

startArg :: Int -> s -> ArgMode s
startArg = ArgMode

addNum :: Int -> ArgMode s -> ArgMode s
addNum n am
    | arg am >= 1000 = am -- shouldn't ever need more than 4 digits
    | otherwise = am {arg = arg am * 10 + n} 

-- todo: negatives
applyArg :: (s -> s) -> ArgMode s -> s
applyArg f am = repeatN (arg am) (argState am)
    where
        repeatN n | n <= 1 = f
                    | otherwise = f . repeatN (n-1)

---------------
data Cleared = Cleared

instance LineState Cleared where
    beforeCursor _ Cleared = ""
    afterCursor Cleared = ""

data Message s = Message {messageState :: s, messageText :: String}

instance LineState s => LineState (Message s) where
    beforeCursor _ = messageText
    afterCursor _ = ""
