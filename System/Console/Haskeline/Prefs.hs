module System.Console.Haskeline.Prefs where

import Language.Haskell.TH
import Data.Char(isSpace,toLower)
import Data.List(foldl')
import Control.Exception(handle)


{- |
'Prefs' allow the user to customize the line-editing interface.  They are
read by default from @~/.haskeline@; to override that behavior, use
'readPrefs' and 'runInputTWithPrefs'.  

Each line of a @.haskeline@ file may define
one field of the 'Prefs' datatype; field names are case-insensitive and
unparseable lines are ignored.  For example:

> editMode: Vi
> completionType: MenuCompletion
> maxhistorysize: Just 40

-}
data Prefs = Prefs { bellStyle :: !BellStyle,
                     editMode :: !EditMode,
                     maxHistorySize :: !(Maybe Int),
                     completionType :: !CompletionType,
                     completionPaging :: !Bool,
                     completionPromptLimit :: !(Maybe Int),
                     listCompletionsImmediately :: !Bool
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

settors :: [(String,String -> Prefs -> Prefs)]
settors = $(do
    DataConI _ _ prefsType _ <- reify 'Prefs
    TyConI (DataD _ _ _ [RecC _ fields] _) <- reify prefsType
    x <- newName "x"
    p <- newName "p"
    -- settor f => ("f", mkSettor (\x p -> p {f=x}))
    let settor (f,_,_) = TupE [LitE (StringL (map toLower $ nameBase f)),
                        AppE (VarE 'mkSettor) $ LamE [VarP x,VarP p]
                        $ RecUpdE (VarE p) [(f,VarE x)]]
    return $ ListE $ map settor fields)

-- | Read 'Prefs' from a given file.  If there is an error reading the file,
-- the 'defaultPrefs' will be returned.
readPrefs :: FilePath -> IO Prefs
readPrefs file = handle (\_ -> return defaultPrefs) $ do
    ls <- fmap lines $ readFile file
    return $ foldl' applyField defaultPrefs ls
  where
    applyField p l = case break (==':') l of
                (name,val)  -> case lookup (map toLower $ trimSpaces name) settors of
                        Nothing -> p
                        Just set -> set (drop 1 val) p  -- drop initial ":", don't crash if val==""
    trimSpaces = dropWhile isSpace . reverse . dropWhile isSpace . reverse
                    
