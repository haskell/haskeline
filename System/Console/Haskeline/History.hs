module System.Console.Haskeline.History(
                        History(),
                        stifleAmount,
                        emptyHistory,
                        addHistory,
                        historyLines,
                        readHistory,
                        writeHistory,
                        stifleHistory
                        ) where

import qualified Data.Sequence as Seq
import Data.Foldable

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import Control.Exception.Extensible

import System.Directory(doesFileExist)

data History = History {histLines :: Seq.Seq String,
                        stifleAmount :: Maybe Int}
                    -- stored in reverse
                    
instance Show History where
    show = show . histLines

emptyHistory :: History
emptyHistory = History Seq.empty Nothing

historyLines :: History -> [String]
historyLines = toList . histLines

-- TODO: am I doing the right thing, error-handling-wise?
-- should probably just silently catch all errors.

-- | Reads the line input history from the given file.  Returns 
-- 'emptyHistory' if the file does not exist or could not be read.
readHistory :: FilePath -> IO History
readHistory file = handle (\(_::IOException) -> return emptyHistory) $ do
    exists <- doesFileExist file
    contents <- if exists
        -- use binary file I/O to avoid Windows CRLF line endings
        -- which cause confusion when switching between systems.
        then fmap UTF8.toString (B.readFile file)
        else return ""
    evaluate (length contents) -- force file closed
    return $ History {histLines = Seq.fromList $ lines contents,
                    stifleAmount = Nothing}

-- | Writes the line history to the given file.  If there is an
-- error when writing the file, it will be ignored.
writeHistory :: FilePath -> History -> IO ()
writeHistory file = handle (\(_::IOException) -> return ())
        . B.writeFile file . UTF8.fromString
        . unlines . historyLines 

stifleHistory :: Maybe Int -> History -> History
stifleHistory Nothing hist = hist {stifleAmount = Nothing}
stifleHistory a@(Just n) hist = History {histLines = stifleFnc (histLines hist),
                                stifleAmount = a}
    where
        stifleFnc = if n > Seq.length (histLines hist)
                        then id
                        else Seq.fromList . take n . toList
                
addHistory :: String -> History -> History
addHistory s h = h {histLines = s Seq.<| stifledLines}
  where
    stifledLines = if maybe True (> Seq.length (histLines h)) (stifleAmount h)
                    then histLines h
                    else case Seq.viewr (histLines h) of
                            Seq.EmptyR -> histLines h -- shouldn't ever happen
                            ls Seq.:> _ -> ls
