module System.Console.HaskLine.Modes where

import System.Console.HaskLine.LineState

class Move s where
    goLeft, goRight, moveToStart, moveToEnd :: s -> s
    

data InsertMode = IMode String String 
                    deriving Show

instance LineState InsertMode where
    beforeCursor prefix (IMode xs _) = prefix ++ reverse xs
    afterCursor (IMode _ ys) = ys
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
deleteNext (IMode xs (y:ys)) = IMode xs ys

deletePrev im@(IMode [] _) = im
deletePrev (IMode (x:xs) ys) = IMode xs ys 


data CommandMode = CMode String Char String | CEmpty
                    deriving Show

instance LineState CommandMode where
    beforeCursor prefix CEmpty = prefix
    beforeCursor prefix (CMode xs _ _) = prefix ++ reverse xs
    afterCursor CEmpty = ""
    afterCursor (CMode _ c ys) = c:ys
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
deleteChar (CMode xs c (y:ys)) = CMode xs y ys
deleteChar (CMode (x:xs) c []) = CMode xs x []
deleteChar _ = CEmpty

replaceChar :: Char -> CommandMode -> CommandMode
replaceChar c (CMode xs _ ys) = CMode xs c ys



------------------------
-- Transitioning between modes

enterCommandMode :: InsertMode -> CommandMode
enterCommandMode (IMode (x:xs) ys) = CMode xs x ys
enterCommandMode _ = CEmpty

insertFromCommandMode, appendFromCommandMode :: CommandMode -> InsertMode

insertFromCommandMode CEmpty = emptyIM
insertFromCommandMode (CMode xs c ys) = IMode xs (c:ys)

appendFromCommandMode CEmpty = emptyIM
appendFromCommandMode (CMode xs c ys) = IMode (c:xs) ys



