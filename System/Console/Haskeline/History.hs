{- |
This module provides a low-level API to the line history stored in the @InputT@ monad transformer.


For most application, it should suffice to instead use the following @Settings@ flags:

  * @autoAddHistory@: add nonblank lines to the command history ('True' by default).

  * @historyFile@: read/write the history to a file before and after the line input session.

If you do want custom history behavior, you may need to disable the above default setting(s).

-}
module System.Console.Haskeline.History(
                        History(),
                        emptyHistory,
                        addHistory,
                        historyLines,
                        readHistory,
                        writeHistory,
                        stifleHistory,
                        stifleAmount,
                        ) where

import qualified Data.Sequence as Seq
import Data.Foldable

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import Control.Exception.Extensible

import System.Directory(doesFileExist)

data History = History {histLines :: Seq.Seq String,
                        stifleAmt :: Maybe Int}
                    -- stored in reverse

-- | The maximum number of lines stored in the history.  If 'Nothing', the history storage is unlimited.
stifleAmount :: History -> Maybe Int
stifleAmount = stifleAmt

instance Show History where
    show = show . histLines

emptyHistory :: History
emptyHistory = History Seq.empty Nothing

-- | The input lines stored in the history (newest first)
historyLines :: History -> [String]
historyLines = toList . histLines

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
    _ <- evaluate (length contents) -- force file closed
    return $ History {histLines = Seq.fromList $ lines contents,
                    stifleAmt = Nothing}

-- | Writes the line history to the given file.  If there is an
-- error when writing the file, it will be ignored.
writeHistory :: FilePath -> History -> IO ()
writeHistory file = handle (\(_::IOException) -> return ())
        . B.writeFile file . UTF8.fromString
        . unlines . historyLines 

-- | Limit the number of lines stored in the history.
stifleHistory :: Maybe Int -> History -> History
stifleHistory Nothing hist = hist {stifleAmt = Nothing}
stifleHistory a@(Just n) hist = History {histLines = stifleFnc (histLines hist),
                                stifleAmt = a}
    where
        stifleFnc = if n > Seq.length (histLines hist)
                        then id
                        else Seq.fromList . take n . toList

addHistory :: String -> History -> History
addHistory s h = h {histLines = s Seq.<| stifledLines}
  where
    stifledLines = if maybe True (> Seq.length (histLines h)) (stifleAmt h)
                    then histLines h
                    else case Seq.viewr (histLines h) of
                            Seq.EmptyR -> histLines h -- shouldn't ever happen
                            ls Seq.:> _ -> ls
