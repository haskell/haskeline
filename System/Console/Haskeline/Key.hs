module System.Console.Haskeline.Key(Key(..),
            Modifier(..),
            BaseKey(..),
            simpleKey,
            simpleChar,
            metaChar,
            ctrlChar,
            parseKey,
            canonicalizeKey) where

import Data.Char
import Control.Monad

-- TODO: should this be [Modifier]?
data Key = Key (Maybe Modifier) BaseKey
            deriving (Show,Eq)

data Modifier = ControlKey | Meta | Shift
            deriving (Show,Eq)

data BaseKey = KeyChar Char
             | FunKey Int
             | LeftKey | RightKey | DownKey | UpKey
             -- TODO: is KillLine really a key?
             | KillLine | Home | End
             | Backspace | Delete
             | Return
             | Tab
             | Clear
             | Escape
            deriving (Show,Eq)

simpleKey :: BaseKey -> Key
simpleKey = Key Nothing

simpleChar, metaChar, ctrlChar :: Char -> Key
simpleChar = Key Nothing . KeyChar
metaChar = Key (Just Meta) . KeyChar
ctrlChar = Key (Just ControlKey) . KeyChar

specialKeys :: [(String,BaseKey)]
specialKeys = [("left",LeftKey)
              ,("right",RightKey)
              ,("down",DownKey)
              ,("up",UpKey)
              ,("killline",KillLine)
              ,("home",Home)
              ,("end",End)
              ,("backspace",Backspace)
              ,("delete",Delete)
              ,("return",Return)
              ,("enter",Return)
              ,("tab",Tab)
              ,("clear",Clear)
              ,("esc",Escape)
              ,("escape",Escape)
              ]

parseModifier :: String -> Maybe Modifier
parseModifier str = case map toLower str of
    "ctrl" -> Just ControlKey
    "control" -> Just ControlKey
    "meta" -> Just Meta
    "shift" -> Just Shift
    _ -> Nothing

parseKey :: String -> Maybe Key
parseKey str = case break (=='-') str of
    (ms,'-':ks) -> liftM2 (Key . Just) (parseModifier ms) (parseBaseKey ks)
    (ks,_) -> liftM (Key Nothing) (parseBaseKey ks)

parseBaseKey :: String -> Maybe BaseKey
parseBaseKey ks = lookup ks specialKeys
                `mplus` parseFunctionKey ks
                `mplus` parseKeyChar ks
    where
        parseKeyChar [c] = Just (KeyChar c)
        parseKeyChar _ = Nothing

        parseFunctionKey ('f':ns) = case reads ns of
            [(n,"")]    -> Just (FunKey n)
            _           -> Nothing
        parseFunctionKey _ = Nothing

canonicalizeKey :: Key -> Key
canonicalizeKey (Key ms (KeyChar '\b')) = Key ms Backspace
canonicalizeKey (Key ms (KeyChar '\t')) = Key ms Tab
canonicalizeKey (Key ms (KeyChar '\r')) = Key ms Return
canonicalizeKey (Key ms (KeyChar '\n')) = Key ms Return
canonicalizeKey (Key ms (KeyChar '\ESC')) = Key ms Escape
canonicalizeKey (Key Nothing (KeyChar c))
    | c <= '\031'   = Key (Just ControlKey) $ KeyChar $ 
                        toEnum (fromEnum c + 96)
canonicalizeKey (Key (Just ControlKey) (KeyChar c)) = case toLower c of
    'h' -> Key Nothing Backspace
    'i' -> Key Nothing Tab
    'j' -> Key Nothing Return
    'm' -> Key Nothing Return
    '[' -> Key Nothing Escape
    lc -> Key (Just ControlKey) (KeyChar lc)
canonicalizeKey (Key (Just Shift) (KeyChar c))
    | isPrint c && isLower c          = Key Nothing (KeyChar (toUpper c))
canonicalizeKey k                   = k

