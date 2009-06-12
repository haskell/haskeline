{- |
This module contains the various datatypes which model the state of the line; that is, the characters displayed and the position of the cursor.
-}
module System.Console.Haskeline.LineState(
                    -- * Graphemes
                    Grapheme(),
                    baseChar,
                    stringToGraphemes,
                    graphemesToString,
                    modifyBaseChar,
                    mapBaseChars,
                    -- * Line State class
                    LineState(..),
                    -- ** Convenience functions for the drawing backends
                    LineChars,
                    lineChars,
                    lengthToEnd,
                    -- ** Supplementary classes
                    Result(..),
                    FromString(..),
                    Move(..),
                    -- * Instances
                    -- ** InsertMode
                    InsertMode(..),
                    emptyIM,
                    insertChar,
                    insertString,
                    replaceCharIM,
                    insertGraphemes,
                    deleteNext,
                    deletePrev,
                    skipLeft,
                    skipRight,
                    deleteFromMove,
                    -- *** Moving to word boundaries
                    goRightUntil,
                    goLeftUntil,
                    atStart,
                    atEnd,
                    -- ** CommandMode
                    CommandMode(..),
                    deleteChar,
                    replaceChar,
                    -- *** Transitioning between modes
                    enterCommandMode,
                    enterCommandModeRight,
                    insertFromCommandMode,
                    appendFromCommandMode,
                    withCommandMode,
                    -- ** ArgMode
                    ArgMode(..),
                    startArg,
                    hiddenArg,
                    addNum,
                    applyArg,
                    applyCmdArg,
                    -- ** Other line state types
                    Cleared(..),
                    Message(..),
                    ) where

import Data.Char

-- | A 'Grapheme' is a fundamental unit of display for the UI.  Several characters in sequence
-- can represent one grapheme; for example, an @a@ followed by the diacritic @\'\\768\'@ should
-- be treated as one unit.
data Grapheme = Grapheme {gBaseChar :: Char,
                            combiningChars :: [Char]}
                    deriving (Show, Eq)

baseChar :: Grapheme -> Char
baseChar = gBaseChar

modifyBaseChar :: (Char -> Char) -> Grapheme -> Grapheme
modifyBaseChar f g = g {gBaseChar = f (gBaseChar g)}

mapBaseChars :: (Char -> Char) -> [Grapheme] -> [Grapheme]
mapBaseChars f = map (modifyBaseChar f)

-- | Create a 'Grapheme' from a single base character.
--
-- NOTE: Careful, don't use outside this module; and inside, make sure this is only
-- ever called on non-combining characters.
baseGrapheme :: Char -> Grapheme
baseGrapheme c = Grapheme {gBaseChar = c, combiningChars = []}

-- | Add a combining character to the given 'Grapheme'.
addCombiner :: Grapheme -> Char -> Grapheme
addCombiner g c = g {combiningChars = combiningChars g ++ [c]}

isCombiningChar :: Char -> Bool
isCombiningChar c = generalCategory c == NonSpacingMark

-- | Converts a string into a sequence of graphemes.
--
-- NOTE: Drops any initial, unattached combining characters.
stringToGraphemes :: String -> [Grapheme]
stringToGraphemes = mkString . dropWhile isCombiningChar
    where
        mkString [] = []
        mkString (c:cs) = Grapheme c (takeWhile isCombiningChar cs)
                                : mkString (dropWhile isCombiningChar cs)

graphemesToString :: [Grapheme] -> String
graphemesToString = concatMap (\g -> (baseChar g : combiningChars g))

-- | This class abstracts away the internal representations of the line state,
-- for use by the drawing actions.  Line state is generally stored in a zipper format.
class LineState s where
    beforeCursor :: String -- ^ The input prefix.
                    -> s -- ^ The current line state.
                    -> [Grapheme] -- ^ The text to the left of the cursor, reversed.  (This 
                                  -- includes the prefix.)
    afterCursor :: s -> [Grapheme] -- ^ The text under and to the right of the cursor.
    isTemporary :: s -> Bool        -- ^ When several lines are printed from a completion
                                    -- attempt, should this state remain on the screen or
                                    -- be cleared?
    isTemporary _ = False

-- | The characters in the line (with the cursor in the middle).  NOT in a zippered format;
-- both lists are in the order left->right that appears on the screen.
type LineChars = ([Grapheme],[Grapheme])

-- | Accessor function for the various backends.
lineChars :: LineState s => String -> s -> LineChars
lineChars prefix s = (beforeCursor prefix s, afterCursor s)

-- | Compute the number of characters under and to the right of the cursor.
lengthToEnd :: LineChars -> Int
lengthToEnd = length . snd

class LineState s => Result s where
    toResult :: s -> String

class (Result s) => FromString s where
    fromString :: String -> s

class Move s where
    goLeft, goRight, moveToStart, moveToEnd :: s -> s
    
-- | The standard line state representation; considers the cursor to be located
-- between two characters.  The first list is reversed.
data InsertMode = IMode [Grapheme] [Grapheme]
                    deriving (Show, Eq)

instance LineState InsertMode where
    beforeCursor prefix (IMode xs _) = stringToGraphemes prefix ++ reverse xs
    afterCursor (IMode _ ys) = ys

instance Result InsertMode where
    toResult (IMode xs ys) = graphemesToString $ reverse xs ++ ys

instance Move InsertMode where
    goLeft im@(IMode [] _) = im 
    goLeft (IMode (x:xs) ys) = IMode xs (x:ys)

    goRight im@(IMode _ []) = im
    goRight (IMode ys (x:xs)) = IMode (x:ys) xs

    moveToStart (IMode xs ys) = IMode [] (reverse xs ++ ys)
    moveToEnd (IMode xs ys) = IMode (reverse ys ++ xs) []

instance FromString InsertMode where
    fromString s = IMode (reverse (stringToGraphemes s)) []

emptyIM :: InsertMode
emptyIM = IMode [] []

-- | Insert one character, which may be combining, to the left of the cursor.
--  
insertChar :: Char -> InsertMode -> InsertMode
insertChar c im@(IMode xs ys)
    | isCombiningChar c = case xs of
                            []   -> im -- drop a combining character if it
                                       -- appears at the start of the line.
                            z:zs -> IMode (addCombiner z c : zs) ys
    | otherwise         = IMode (baseGrapheme c : xs) ys

-- | Insert a sequence of characters to the left of the cursor. 
insertString :: String -> InsertMode -> InsertMode
insertString s (IMode xs ys) = IMode (reverse (stringToGraphemes s) ++ xs) ys

deleteNext, deletePrev :: InsertMode -> InsertMode
deleteNext im@(IMode _ []) = im
deleteNext (IMode xs (_:ys)) = IMode xs ys

deletePrev im@(IMode [] _) = im
deletePrev (IMode (_:xs) ys) = IMode xs ys 

skipLeft, skipRight :: (Char -> Bool) -> InsertMode -> InsertMode
skipLeft f (IMode xs ys) = let (ws,zs) = span (f . baseChar) xs 
                           in IMode zs (reverse ws ++ ys)
skipRight f (IMode xs ys) = let (ws,zs) = span (f . baseChar) ys 
                            in IMode (reverse ws ++ xs) zs

insertGraphemes :: [Grapheme] -> InsertMode -> InsertMode
insertGraphemes s (IMode xs ys) = IMode (reverse s ++ xs) ys

-- For the 'R' command.
replaceCharIM :: Char -> InsertMode -> InsertMode
replaceCharIM c im
    | isCombiningChar c = case im of
                    IMode [] [] -> im
                    IMode [] (y:ys) -> IMode [] (addCombiner y c:ys)
                    IMode (x:xs) ys -> IMode (addCombiner x c:xs) ys
    | otherwise = let g = baseGrapheme c
                  in case im of
                    IMode xs [] -> IMode (g:xs) []
                    IMode xs (_:ys) -> IMode (g:xs) ys


-- | Used by vi mode.  Considers the cursor to be located over some specific character.
-- The first list is reversed.
data CommandMode = CMode [Grapheme] Grapheme [Grapheme] | CEmpty
                    deriving Show

instance LineState CommandMode where
    beforeCursor prefix CEmpty = stringToGraphemes prefix
    beforeCursor prefix (CMode xs _ _) = stringToGraphemes prefix ++ reverse xs
    afterCursor CEmpty = []
    afterCursor (CMode _ c ys) = c:ys

instance Result CommandMode where
    toResult CEmpty = ""
    toResult (CMode xs c ys) = graphemesToString $ reverse xs ++ (c:ys)

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
    fromString s = case reverse (stringToGraphemes s) of
                    [] -> CEmpty
                    (c:cs) -> CMode cs c []

deleteChar :: CommandMode -> CommandMode
deleteChar (CMode xs _ (y:ys)) = CMode xs y ys
deleteChar (CMode (x:xs) _ []) = CMode xs x []
deleteChar _ = CEmpty

replaceChar :: Char -> CommandMode -> CommandMode
replaceChar c (CMode xs d ys)
    | not (isCombiningChar c)   = CMode xs (baseGrapheme c) ys
    | otherwise                 = CMode xs (addCombiner d c) ys
replaceChar _ CEmpty = CEmpty

------------------------
-- Transitioning between modes

enterCommandMode, enterCommandModeRight :: InsertMode -> CommandMode
enterCommandMode (IMode (x:xs) ys) = CMode xs x ys
enterCommandMode (IMode [] (y:ys)) = CMode [] y ys
enterCommandMode _ = CEmpty

enterCommandModeRight (IMode xs (y:ys)) = CMode xs y ys
enterCommandModeRight (IMode (x:xs) []) = CMode xs x []
enterCommandModeRight _ = CEmpty


insertFromCommandMode, appendFromCommandMode :: CommandMode -> InsertMode

insertFromCommandMode CEmpty = emptyIM
insertFromCommandMode (CMode xs c ys) = IMode xs (c:ys)

appendFromCommandMode CEmpty = emptyIM
appendFromCommandMode (CMode xs c ys) = IMode (c:xs) ys

withCommandMode :: (InsertMode -> InsertMode) -> CommandMode -> CommandMode
withCommandMode f = enterCommandModeRight . f . insertFromCommandMode

----------------------
-- Supplementary modes

-- | Used for commands which take an integer argument.
data ArgMode s = ArgMode {arg :: Int, argState :: s,
                            overwritePrefix :: Bool}

instance LineState s => LineState (ArgMode s) where
    beforeCursor prefix am = let pre' = if overwritePrefix am
                                            then "(arg: " ++ show (arg am) ++ ") "
                                            else prefix
                             in beforeCursor pre' (argState am) 
    afterCursor = afterCursor . argState

instance Result s => Result (ArgMode s) where
    toResult = toResult . argState

startArg :: Int -> s -> ArgMode s
startArg n s = ArgMode n s True

addNum :: Int -> ArgMode s -> ArgMode s
addNum n am
    | arg am >= 1000 = am -- shouldn't ever need more than 4 digits
    | otherwise = am {arg = arg am * 10 + n} 

hiddenArg :: Int -> s -> ArgMode s
hiddenArg n s = ArgMode n s False

-- todo: negatives
applyArg :: (s -> s) -> ArgMode s -> s
applyArg f am = repeatN (arg am) f (argState am)

repeatN :: Int -> (a -> a) -> a -> a
repeatN n f | n <= 1 = f
          | otherwise = f . repeatN (n-1) f

applyCmdArg :: (InsertMode -> InsertMode) -> ArgMode CommandMode -> CommandMode
applyCmdArg f am = withCommandMode (repeatN (arg am) f) (argState am)

---------------
data Cleared = Cleared

instance LineState Cleared where
    beforeCursor _ Cleared = []
    afterCursor Cleared = []

data Message s = Message {messageState :: s, messageText :: String}

instance LineState s => LineState (Message s) where
    beforeCursor _ = stringToGraphemes . messageText
    afterCursor _ = []
    isTemporary _ = True

-----------------

deleteFromDiff :: InsertMode -> InsertMode -> InsertMode
deleteFromDiff (IMode xs1 ys1) (IMode xs2 ys2)
    | length xs1 < length xs2 = IMode xs1 ys2 -- moved right
    | otherwise = IMode xs2 ys1

deleteFromMove :: (InsertMode -> InsertMode) -> InsertMode -> InsertMode
deleteFromMove f = \x -> deleteFromDiff x (f x)

atStart, atEnd :: (Char -> Bool) -> InsertMode -> Bool
atStart f (IMode (x:_) (y:_)) = not (f (baseChar x)) && f (baseChar y)
atStart _ _ = False

atEnd f (IMode _ (y1:y2:_)) = f (baseChar y1) && not (f (baseChar y2))
atEnd _ _ = False

goRightUntil, goLeftUntil :: (InsertMode -> Bool) -> InsertMode -> InsertMode
goRightUntil f = loop . goRight
    where
        loop im@(IMode _ ys) | null ys || f im  = im
                             | otherwise = loop (goRight im)
goLeftUntil f = loop . goLeft
    where
        loop im@(IMode xs _)   | null xs || f im = im
                            | otherwise = loop (goLeft im)

