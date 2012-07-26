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

-- Way-dependent imports
#ifdef USE_GHC_ENCODINGS
import GHC.IO.Encoding (initLocaleEncoding)
import System.Console.Haskeline.Recover
#else
import System.Console.Haskeline.Backend.Posix.IConv
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
#ifdef TERMINFO
import qualified Data.ByteString.Char8 as BC
#endif
import Control.Monad (liftM2)
#endif



#ifdef USE_GHC_ENCODINGS
data Encoder = Encoder
data Decoder = Decoder
#else
type Decoder = PartialDecoder
type Encoder = String -> IO ByteString
#endif

newEncoders :: IO (Encoder,Decoder)
#ifdef USE_GHC_ENCODINGS
newEncoders = return (Encoder,Decoder)
#else
newEncoders = do
    codeset <- bracket (setLocale (Just "")) setLocale $ const $ getCodeset
    liftM2 (,) (openEncoder codeset)
                (openPartialDecoder codeset)
#endif

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
#ifdef USE_GHC_ENCODINGS
withCodingMode (ExternalHandle OtherMode h) act = do
    bracket (liftIO $ hGetEncoding h)
            (liftIO . hSetBinOrEncoding h)
            $ const $ do
                hSetEncoding h haskelineEncoding
                act

hSetBinOrEncoding :: Handle -> Maybe TextEncoding -> IO ()
hSetBinOrEncoding h Nothing = hSetBinaryMode h True
hSetBinOrEncoding h (Just enc) = hSetEncoding h enc
#else
withCodingMode (ExternalHandle OtherMode h) act = hWithBinaryMode h act
#endif

#ifdef USE_GHC_ENCODINGS
haskelineEncoding :: TextEncoding
haskelineEncoding = transliterateFailure initLocaleEncoding
#endif

-- Open a file and permanently set it to the correct mode.
openInCodingMode :: FilePath -> IOMode -> IO ExternalHandle
#ifdef USE_GHC_ENCODINGS
openInCodingMode path iomode = do
    h <- openFile path iomode
    hSetEncoding h haskelineEncoding
    return $ ExternalHandle CodingMode h
#else
openInCodingMode path iomode
    = fmap (ExternalHandle CodingMode) $ openBinaryFile path iomode
#endif



-----------------------
-- Output
putEncodedStr :: Encoder -> Handle -> String -> IO ()
#ifdef USE_GHC_ENCODINGS
putEncodedStr _ h = hPutStr h
#else
putEncodedStr enc h s = enc s >>= B.hPutStr h
#endif

#ifdef TERMINFO
getTermText :: Encoder -> String -> IO Terminfo.TermOutput
#ifdef USE_GHC_ENCODINGS
getTermText _ = return . Terminfo.termText
#else
getTermText enc s = enc s >>= return . Terminfo.termText . BC.unpack
#endif
#endif



-- Read at least one character of input, and more if immediately
-- available.  In particular the characters making up a control sequence
-- will all be available at once, so they can be processed together
-- (with Posix.lexKeys).
getBlockOfChars :: Handle -> Decoder -> IO String
#ifdef USE_GHC_ENCODINGS
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
#else
getBlockOfChars h decode = do
    let bufferSize = 32
    blockUntilInput h
    bs <- B.hGetNonBlocking h bufferSize
    decodeAndMore decode h bs

#endif

-- Read in a single character, or Nothing if eof.
-- Assumes the handle is "prepared".
getDecodedChar :: Handle -> Decoder -> MaybeT IO Char
#ifdef USE_GHC_ENCODINGS
getDecodedChar h _ = guardedEOF hGetChar h
#else
getDecodedChar h decode = do
    b <- hGetByte h
    cs <- liftIO $ decodeAndMore decode h (B.pack [b])
    case cs of
        [] -> return '?' -- shouldn't happen, but doesn't hurt to be careful.
        (c:_) -> return c
#endif

-- Read in a single line, or Nothing if eof.
getDecodedLine :: Handle -> Decoder -> MaybeT IO String
#ifdef USE_GHC_ENCODINGS
getDecodedLine h _ = guardedEOF hGetLine h
#else
getDecodedLine h decode
    = hGetLocaleLine h >>= liftIO . decodeAndMore decode h

#endif

-- Helper functions for iconv encoding
#ifndef USE_GHC_ENCODINGS
blockUntilInput :: Handle -> IO ()
#if __GLASGOW_HASKELL__ >= 611
-- threadWaitRead doesn't work with the new (ghc-6.12) IO library,
-- because it keeps a buffer even when NoBuffering is set.
blockUntilInput h = hWaitForInput h (-1) >> return ()
#else
-- hWaitForInput doesn't work with -threaded on ghc < 6.10
-- (#2363 in ghc's trac)
blockUntilInput h = unsafeHandleToFD h >>= threadWaitRead . Fd
#endif

#endif


