module System.Console.Haskeline.Settings where

import Language.Haskell.TH
import Data.Char(isSpace,toLower)
import Data.List(foldl')

-- | Performs completions from a reversed 'String'.  The output 'String' is also reversed.
-- In general, this can be built using 'completeWord'.
type CompletionFunc m = String -> m (String, [Completion])


data Completion = Completion {replacement, display :: String}
                    deriving Show


data Settings m = Settings {complete :: CompletionFunc m,
                            historyFile :: Maybe String,
                            handleSigINT :: Bool}

-- | Because 'complete' is the only field of 'Settings' depending on @m@,
-- the expression @defaultSettings {completionFunc = f}@ leads to a type error
-- from being too general.  This function may become unnecessary if another field
-- depending on @m@ is added.
setComplete :: CompletionFunc m -> Settings m -> Settings m
setComplete f s = s {complete = f}

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

readPrefs :: FilePath -> IO Prefs
readPrefs file = do
    ls <- fmap lines $ readFile file
    return $ foldl' applyField defaultPrefs ls
  where
    applyField p l = case break (==':') l of
                (name,val)  -> case lookup (map toLower $ trimSpaces name) settors of
                        Nothing -> p
                        Just set -> set (drop 1 val) p  -- drop initial ":", don't crash if val==""
    trimSpaces = dropWhile isSpace . reverse . dropWhile isSpace . reverse
                    
