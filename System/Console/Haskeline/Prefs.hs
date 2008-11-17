{- |
'Prefs' allow the user to customize the line-editing interface.  They are
read by default from @~/.haskeline@; to override that behavior, use
'readPrefs' and @runInputTWithPrefs@.  

Each line of a @.haskeline@ file defines
one field of the 'Prefs' datatype; field names are case-insensitive and
unparseable lines are ignored.  For example:

> editMode: Vi
> completionType: MenuCompletion
> maxhistorysize: Just 40

-}
module System.Console.Haskeline.Prefs(
                        Prefs(..),
                        defaultPrefs,
                        readPrefs,
                        CompletionType(..),
                        BellStyle(..),
                        EditMode(..)
                        ) where

import Data.Char(isSpace,toLower)
import Data.List(foldl')
import System.Console.Haskeline.MonadException(handle,IOException)


data Prefs = Prefs { bellStyle :: !BellStyle,
                     editMode :: !EditMode,
                     maxHistorySize :: !(Maybe Int),
                     completionType :: !CompletionType,
                     completionPaging :: !Bool, 
                        -- ^ When listing completion alternatives, only display
                        -- one screen of possibilities at a time.
                     completionPromptLimit :: !(Maybe Int),
                        -- ^ If more than this number of completion
                        -- possibilities are found, then ask before listing
                        -- them.
                     listCompletionsImmediately :: !Bool
                        -- ^ If 'False', completions with multiple possibilities
                        -- will ring the bell and only display them if the user
                        -- presses @TAB@ again.
                     }
                        deriving (Read,Show)

data CompletionType = ListCompletion | MenuCompletion
            deriving (Read,Show)


data BellStyle = NoBell | VisualBell | AudibleBell
                    deriving (Show, Read)

data EditMode = Vi | Emacs
                    deriving (Show,Read)

{- | The default preferences which may be overwritten in the @.haskeline@ file:

> defaultPrefs = Prefs {bellStyle = AudibleBell,
>                      maxHistorySize = Nothing,
>                      editMode = Emacs,
>                      completionType = ListCompletion,
>                      completionPaging = True,
>                      completionPromptLimit = Just 100,
>                      listCompletionsImmediately = True
>                    }

-}
defaultPrefs :: Prefs
defaultPrefs = Prefs {bellStyle = AudibleBell,
                      maxHistorySize = Nothing,
                      editMode = Emacs,
                      completionType = ListCompletion,
                      completionPaging = True,
                      completionPromptLimit = Just 100,
                      listCompletionsImmediately = True
                    }

mkSettor :: Read a => (a -> Prefs -> Prefs) -> String -> Prefs -> Prefs
mkSettor f str = case reads str of
                [(x,_)] -> f x
                _ -> id

settors :: [(String, String -> Prefs -> Prefs)]
settors = [("bellstyle", mkSettor $ \x p -> p {bellStyle = x})
          ,("editmode", mkSettor $ \x p -> p {editMode = x})
          ,("maxhistorysize", mkSettor $ \x p -> p {maxHistorySize = x})
          ,("completiontype", mkSettor $ \x p -> p {completionType = x})
          ,("completionpaging", mkSettor $ \x p -> p {completionPaging = x})
          ,("completionpromptlimit", mkSettor $ \x p -> p {completionPromptLimit = x})
          ,("listcompletionsimmediately", mkSettor $ \x p -> p {listCompletionsImmediately = x})

          ]

-- | Read 'Prefs' from a given file.  If there is an error reading the file,
-- the 'defaultPrefs' will be returned.
readPrefs :: FilePath -> IO Prefs
readPrefs file = handle (\(_::IOException) -> return defaultPrefs) $ do
    ls <- fmap lines $ readFile file
    return $ foldl' applyField defaultPrefs ls
  where
    applyField p l = case break (==':') l of
                (name,val)  -> case lookup (map toLower $ trimSpaces name) settors of
                        Nothing -> p
                        Just set -> set (drop 1 val) p  -- drop initial ":", don't crash if val==""
    trimSpaces = dropWhile isSpace . reverse . dropWhile isSpace . reverse
                    
