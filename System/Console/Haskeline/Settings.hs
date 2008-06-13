module System.Console.Haskeline.Settings where

-- | Performs completions from a reversed 'String'.  The output 'String' is also reversed.
-- In general, this can be built using 'completeWord'.
type CompletionFunc m = String -> m (String, [Completion])


data Completion = Completion {replacement, display :: String}
                    deriving Show


data CompletionType = MenuCompletion
                    -- ^ Cycle through the alternatives one at a time
                    | ListCompletions { -- TODO: listChoicesImmediately :: Bool,
                                  usePaging :: Bool,
                                  askBeforeListing :: Maybe Int
                                    -- ^ If this is set to @Just n@, then warn
                                    -- the user before listing more than @n@
                                    -- alternatives.
                                }
                deriving (Show,Read)


data Settings m = Settings {complete :: CompletionFunc m,
                            historyFile :: Maybe String,
                            maxHistorySize :: Maybe Int,
                            handleSigINT :: Bool}

-- | Because 'complete' is the only field of 'Settings' depending on @m@,
-- the expression @defaultSettings {completionFunc = f}@ leads to a type error
-- from being too general.  This function may become unnecessary if another field
-- depending on @m@ is added.
setComplete :: CompletionFunc m -> Settings m -> Settings m
setComplete f s = s {complete = f}

data Prefs = Prefs { bellStyle :: BellStyle,
                     editMode :: EditMode,
                     completionType :: CompletionType
                     }
                        deriving (Read,Show)

data BellStyle = NoBell | VisibleBell | AudibleBell
                    deriving (Show, Read)

data EditMode = Vi | Emacs
                    deriving (Show,Read)

defaultPrefs :: Prefs
defaultPrefs = Prefs {bellStyle = AudibleBell,
                      editMode = Emacs,
                      completionType = ListCompletions {
                                usePaging = True,
                                askBeforeListing = Just 100
                                }
                    }

-- better to have syntax beyond Read instance.
readPrefs :: FilePath -> IO Prefs
readPrefs = fmap read . readFile



