{- | This module provides a wrapper for I/O encoding for the "old" and "new" ways.
The "old" way uses iconv+utf8-string.
The "new" way uses the base library's built-in encoding functionality.
For the "new" way, we require ghc>=7.4.1 due to GHC bug #5436.

This module exports opaque Encoder/Decoder datatypes, along with several helper
functions that wrap the old/new ways.
-}
module System.Console.Haskeline.Backend.Posix.Encoder (
        Encoder,
        Decoder,
        newEncoders,
        ExternalHandle(eH),
        externalHandle,
        withCodingMode,
        openInCodingMode,
        putEncodedStr,
#ifdef TERMINFO
        getTermText,
#endif
        getBlockOfChars,
        getDecodedChar,
        getDecodedLine,
                ) where

import System.IO
import System.Console.Haskeline.Monads
import System.Console.Haskeline.Term
#ifdef TERMINFO
import qualified System.Console.Terminfo.Base as Terminfo
#endif

import GHC.IO.Encoding (initLocaleEncoding)
import System.Console.Haskeline.Recover


data Encoder = Encoder
data Decoder = Decoder

newEncoders :: IO (Encoder,Decoder)
newEncoders = return (Encoder,Decoder)

-- | An 'ExternalHandle' is a handle which may or may not be in the correct
-- mode for Unicode input/output.  When the POSIX backend opens a file
-- (or /dev/tty) it sets it permanently to the correct mode.
-- However, when it uses an existing handle like stdin, it only temporarily
-- sets it to the correct mode (e.g., for the duration of getInputLine);
-- otherwise, we might interfere with the rest of the Haskell program.
--
-- For the legacy backend, the correct mode is BinaryMode.
-- For the new backend, the correct mode is the locale encoding, set to
-- transliterate errors (rather than crashing, as is the base library's
-- default.)  (See Posix/Recover.hs)
data ExternalHandle = ExternalHandle
                        { externalMode :: ExternalMode
                        , eH :: Handle
                        }

data ExternalMode = CodingMode | OtherMode

externalHandle :: Handle -> ExternalHandle
externalHandle = ExternalHandle OtherMode

-- | Use to ensure that an external handle is in the correct mode
-- for the duration of the given action.
withCodingMode :: ExternalHandle -> IO a -> IO a
withCodingMode ExternalHandle {externalMode=CodingMode} act = act
withCodingMode (ExternalHandle OtherMode h) act = do
    bracket (liftIO $ hGetEncoding h)
            (liftIO . hSetBinOrEncoding h)
            $ const $ do
                hSetEncoding h haskelineEncoding
                act

hSetBinOrEncoding :: Handle -> Maybe TextEncoding -> IO ()
hSetBinOrEncoding h Nothing = hSetBinaryMode h True
hSetBinOrEncoding h (Just enc) = hSetEncoding h enc

haskelineEncoding :: TextEncoding
haskelineEncoding = transliterateFailure initLocaleEncoding

-- Open a file and permanently set it to the correct mode.
openInCodingMode :: FilePath -> IOMode -> IO ExternalHandle
openInCodingMode path iomode = do
    h <- openFile path iomode
    hSetEncoding h haskelineEncoding
    return $ ExternalHandle CodingMode h


-----------------------
-- Output
putEncodedStr :: Encoder -> Handle -> String -> IO ()
putEncodedStr _ h = hPutStr h

#ifdef TERMINFO
getTermText :: Encoder -> String -> IO Terminfo.TermOutput
getTermText _ = return . Terminfo.termText
#endif



-- Read at least one character of input, and more if immediately
-- available.  In particular the characters making up a control sequence
-- will all be available at once, so they can be processed together
-- (with Posix.lexKeys).
getBlockOfChars :: Handle -> Decoder -> IO String
getBlockOfChars h _ = do
    c <- hGetChar h
    loop [c]
  where
    loop cs = do
        isReady <- hReady h
        if not isReady
            then return $ reverse cs
            else do
                    c <- hGetChar h
                    loop (c:cs)

-- Read in a single character, or Nothing if eof.
-- Assumes the handle is "prepared".
getDecodedChar :: Handle -> Decoder -> MaybeT IO Char
getDecodedChar h _ = guardedEOF hGetChar h

-- Read in a single line, or Nothing if eof.
getDecodedLine :: Handle -> Decoder -> MaybeT IO String
getDecodedLine h _ = guardedEOF hGetLine h
