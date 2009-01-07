module System.Console.Haskeline.Backend.IConv(
        getCodeset,
        openEncoder,
        openDecoder
        ) where

import Foreign.C
import Foreign
import Data.ByteString (ByteString, useAsCStringLen, empty, append)
import Data.ByteString.Internal (createAndTrim)
import Control.Monad(when)
import qualified Data.ByteString.UTF8 as UTF8

import System.Console.Haskeline.MonadException

#include <locale.h>
#include <langinfo.h>
#include <iconv.h>

openEncoder :: String -> IO (String -> IO ByteString)
openEncoder codeset = do
    encodeT <- iconvOpen codeset "UTF-8"
    return $ iconv encodeT . UTF8.fromString

openDecoder :: String -> IO (ByteString -> IO String)
openDecoder codeset = do
    decodeT <- iconvOpen "UTF-8" codeset
    return $ fmap UTF8.toString . iconv decodeT

---------------------
-- Setting the locale

foreign import ccall "setlocale" c_setlocale :: CInt -> CString -> IO CString

getCodeset :: IO String
getCodeset = bracket setEnvLocale setLocale (const nlLangInfo)
    where
        setLocale = c_setlocale (#const LC_CTYPE)
        setEnvLocale = withCString "" setLocale

-----------------
-- Getting the encoding

type NLItem = #type nl_item

foreign import ccall nl_langinfo :: NLItem -> IO CString

nlLangInfo :: IO String
nlLangInfo = nl_langinfo (#const CODESET) >>= peekCString

----------------
-- Iconv

-- TODO: This may not work on platforms where iconv_t is not a pointer.
type IConvT = ForeignPtr ()
type IConvTPtr = Ptr ()

foreign import ccall iconv_open :: CString -> CString -> IO IConvTPtr

iconvOpen :: String -> String -> IO IConvT
iconvOpen destName srcName = withCString destName $ \dest ->
                            withCString srcName $ \src -> do
                                res <- iconv_open dest src
                                if res == nullPtr `plusPtr` (-1)
                                    then throwErrno "iconvOpen"
                                    else newForeignPtr iconv_close res

-- really this returns a CInt, but it's easiest to just ignore that, I think.
foreign import ccall "&" iconv_close :: FunPtr (IConvTPtr -> IO ())

foreign import ccall "iconv" c_iconv :: IConvTPtr -> Ptr CString -> Ptr CSize
                            -> Ptr CString -> Ptr CSize -> IO CSize

iconv :: IConvT -> ByteString -> IO ByteString
iconv cd inStr = useAsCStringLen inStr $ \(inPtr, inBuffLen) ->
        with inPtr $ \inBuff ->
        with (toEnum inBuffLen) $ \inBytesLeft -> do
                out <- loop inBuffLen (castPtr inBuff) inBytesLeft
                return out
    where
        -- TODO: maybe a better algorithm for increasing the buffer size?
        -- and also maybe a different starting buffer size?
        biggerBuffer = (+1)
        loop outSize inBuff inBytesLeft = do
            bytesLeft <- peek inBytesLeft
            if bytesLeft <= 0
                then return empty
                else do
                    bs <- partialIconv cd outSize inBuff inBytesLeft
                    bs' <- loop (biggerBuffer outSize) inBuff inBytesLeft
                    return (bs `append` bs')

partialIconv :: IConvT -> Int -> Ptr CString -> Ptr CSize -> IO ByteString
partialIconv cd outSize inBuff inBytesLeft =
    withForeignPtr cd $ \cd_p ->
    createAndTrim outSize $ \outPtr ->
    with outPtr $ \outBuff ->
    with (toEnum outSize) $ \outBytesLeft -> do
        ret <- c_iconv cd_p inBuff inBytesLeft
                            (castPtr outBuff) outBytesLeft
        -- if there was a problem converting (either incomplete or invalid errs)
        -- skip the next unprocessed byte.
        when (ret == -1) $ do
            errno <- getErrno
            when (errno /= e2BIG) $ do
                modifyPtr (`plusPtr` 1) inBuff
                modifyPtr (subtract 1) inBytesLeft
        outLeft <- fmap fromEnum $ peek outBytesLeft
        return (outSize - outLeft)

modifyPtr :: Storable a => (a -> a) -> Ptr a -> IO ()
modifyPtr f p = peek p >>= poke p . f
