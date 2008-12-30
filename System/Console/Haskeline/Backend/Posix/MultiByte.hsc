module System.Console.Haskeline.Backend.Posix.MultiByte(
        multiByteToUnicode,
        unicodeToMultiByte,
        getLocale,
        setLocale,
        setLocaleEnv,
        lcLang
        ) where

import Foreign.C
import Foreign

import Data.ByteString (ByteString, useAsCString)
import qualified Data.ByteString as B
import Data.ByteString.Internal (createAndTrim)

#include <locale.h>

foreign import ccall mbstowcs :: CWString -> CString -> CSize -> IO CSize

foreign import ccall setlocale :: CInt -> CString -> IO CString

multiByteToUnicode :: ByteString -> IO String
multiByteToUnicode bs = useAsCString bs $ \inBuff -> do
    outSize <- throwErrnoIfMinus1 "multiByteToUnicode"
                $ mbstowcs nullPtr inBuff 0
    allocaArray0 (fromEnum outSize) $ \outBuff -> do
    outLen <- throwErrnoIfMinus1 "multiByteToUnicode"
                $ mbstowcs outBuff inBuff outSize
    peekCWStringLen (outBuff, fromEnum outLen)

foreign import ccall wcstombs :: CString -> CWString -> CSize -> IO CSize

unicodeToMultiByte :: String -> IO ByteString
unicodeToMultiByte str = withCWString str $ \inBuff -> do
    outSize <- throwErrnoIfMinus1 "unicodeToMultiByte"
                $ wcstombs nullPtr inBuff 0
    createAndTrim (fromEnum outSize) $ \outBuff -> do
        fmap fromEnum $ throwErrnoIfMinus1 "unicodeToMultiByte"
                $ wcstombs (castPtr outBuff) inBuff outSize



getLocale :: CInt -> IO String
getLocale cat = setlocale cat nullPtr >>= peekCString

setLocale :: CInt -> String -> IO String
setLocale cat str = withCString str (setlocale cat) >>= peekCString

setEnvLocale :: CInt -> IO String
setEnvLocale cat = setLocale cat ""

lcLang :: CInt
lcLang = #const LC_CTYPE



