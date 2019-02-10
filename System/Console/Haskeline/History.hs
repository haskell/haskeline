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
                        addHistoryUnlessConsecutiveDupe,
                        addHistoryRemovingAllDupes,
                        historyLines,
                        readHistory,
                        writeHistory,
                        stifleHistory,
                        stifleAmount,
                        ) where

import Control.Exception
import Control.Monad (when)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.Sequence ( Seq, (<|), ViewL(..), ViewR(..), viewl, viewr )
import System.Directory(doesFileExist, renameFile, removeFile)
import System.FilePath ((</>), (<.>), splitFileName)
import qualified System.IO as IO

import System.Console.Haskeline.Recover

data History = History
    { oldLines :: Seq String
    , newLines :: Seq String
    , stifleAmt :: Maybe Int
    }  -- Lines are stored in reverse (newest first).
    deriving Show

-- | The maximum number of lines stored in the history.  If 'Nothing', the history storage is unlimited.
stifleAmount :: History -> Maybe Int
stifleAmount = stifleAmt

emptyHistory :: History
emptyHistory = History Seq.empty Seq.empty Nothing

-- | The input lines stored in the history (newest first)
historyLines :: History -> [String]
historyLines h = toList $ newLines h Seq.>< oldLines h

-- | Reads the line input history from the given file.  Returns
-- 'emptyHistory' if the file does not exist or could not be read.
readHistory :: FilePath -> IO History
readHistory file = handle (\(_::IOException) -> return emptyHistory) $ do
    exists <- doesFileExist file
    contents <- if exists
        then readUTF8File file
        else return ""
    _ <- evaluate (length contents) -- force file closed
    return History
        { oldLines = Seq.fromList $ lines contents
        , newLines = Seq.empty
        , stifleAmt = Nothing
        }

-- | Writes the line history to the given file.  If there is an
-- error when writing the file, it will be ignored.
--
-- This function is implemented by {hist}writing a temporary file named
-- @{file}.XXXX@, and then renaming it to @{file}.
writeHistory :: FilePath -> History -> IO ()
writeHistory file hist = handle (\(_::IOException) -> return ()) $
    bracket (IO.openTempFile dirPart template) cleanup $ \(tmp, h) -> do
        putUTF8Contents h $ unlines $ historyLines hist
        IO.hClose h
        renameFile tmp file
  where
    (dirPart, filePart) = splitFileName file
    template = dirPart </> filePart <.> "XXXX"
    cleanup (tmp, h) = do
        IO.hClose h -- Repeated hClose calls are OK
        exists <- doesFileExist tmp
        when exists $ removeFile tmp

-- | Limit the number of lines stored in the history.
stifleHistory :: Maybe Int -> History -> History
stifleHistory Nothing hist = hist {stifleAmt = Nothing}
stifleHistory a@(Just n) hist = History
    { stifleAmt = a
    , oldLines = Seq.take n (oldLines hist)
    , newLines = Seq.take (n - length (oldLines hist)) $ newLines hist
    }

addHistory :: String -> History -> History
addHistory s h = truncated { newLines = s <| newLines truncated }
  where
    truncated = case stifleAmt h of
        Just n
            | length (newLines h) >= n -> h
                { oldLines = Seq.empty
                , newLines = Seq.take (n-1) (newLines h)
                }
            | length (oldLines h) + length (newLines h) >= n -> h
                { newLines = Seq.take (n - 1 - length (oldLines h)) $ newLines h }
        _ -> h

-- | Add a line to the history unless it matches the previously recorded line.
addHistoryUnlessConsecutiveDupe :: String -> History -> History
addHistoryUnlessConsecutiveDupe h hs = case historyLines hs of
    h1 : _ | h==h1   -> hs
    _                -> addHistory h hs

-- | Add a line to the history, and remove all previous entries which are the 
-- same as it.
addHistoryRemovingAllDupes :: String -> History -> History
addHistoryRemovingAllDupes h hs = addHistory h hs
    { oldLines = filterDupes $ oldLines hs
    , newLines = filterDupes $ newLines hs
    }
  where
    filterDupes = Seq.fromList . filter (/= h) . toList

---------
-- UTF-8 file I/O, for old versions of GHC

readUTF8File :: FilePath -> IO String
readUTF8File file = do
    h <- IO.openFile file IO.ReadMode
    IO.hSetEncoding h $ transliterateFailure IO.utf8
    IO.hSetNewlineMode h IO.noNewlineTranslation
    contents <- IO.hGetContents h
    _ <- evaluate (length contents)
    IO.hClose h
    return contents

putUTF8Contents :: IO.Handle -> String -> IO ()
putUTF8Contents h contents = do
    IO.hSetEncoding h IO.utf8
    -- Write a file which is portable between systems.
    IO.hSetNewlineMode h IO.noNewlineTranslation
    IO.hPutStr h contents
