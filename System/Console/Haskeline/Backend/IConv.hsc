module System.Console.Haskeline.Backend.IConv(
        setLocale,
        getCodeset,
        openEncoder,
        openDecoder,
        openPartialDecoder,
        Result(..)
        ) where

import Foreign.C
import Foreign
import Data.ByteString (ByteString, useAsCStringLen, append)
import Data.ByteString.Internal (createAndTrim')
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8

#include <locale.h>
#include <langinfo.h>
#include <iconv.h>

openEncoder :: String -> IO (String -> IO ByteString)
openEncoder codeset = do
    encodeT <- iconvOpen codeset "UTF-8"
    return $ simpleIConv encodeT . UTF8.fromString

openDecoder :: String -> IO (ByteString -> IO String)
openDecoder codeset = do
    decodeT <- iconvOpen "UTF-8" codeset
    return $ fmap UTF8.toString . simpleIConv decodeT

-- handle errors by dropping unuseable chars.
simpleIConv :: IConvT -> ByteString -> IO ByteString
simpleIConv t bs = do
    (cs,result) <- iconv t bs
    case result of
        Invalid rest    -> fmap (cs `append`) $ simpleIConv t (B.drop 1 rest)
        _               -> return cs

openPartialDecoder :: String -> IO (ByteString -> IO (String, Result))
openPartialDecoder codeset = do
    decodeT <- iconvOpen "UTF-8" codeset
    return $ \bs -> do
        (s,result) <- iconv decodeT bs
        return (UTF8.toString s,result)

---------------------
-- Setting the locale

foreign import ccall "setlocale" c_setlocale :: CInt -> CString -> IO CString

setLocale :: Maybe String -> IO (Maybe String)
setLocale oldLocale = (maybeWith withCString) oldLocale $ \loc_p -> do
    c_setlocale (#const LC_CTYPE) loc_p >>= maybePeek peekCString

-----------------
-- Getting the encoding

type NLItem = #type nl_item

foreign import ccall nl_langinfo :: NLItem -> IO CString

getCodeset :: IO String
getCodeset = nl_langinfo (#const CODESET) >>= peekCString
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

data Result = Successful
            | Invalid ByteString
            | Incomplete ByteString
    deriving Show

iconv :: IConvT -> ByteString -> IO (ByteString,Result)
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
            (bs, errno) <- partialIconv cd outSize inBuff inBytesLeft
            inLeft <- fmap fromEnum $ peek inBytesLeft
            let rest = B.drop (B.length inStr - inLeft) inStr
            case errno of
                Nothing -> return (bs,Successful)
                Just err 
                    | err == e2BIG  -> do -- output buffer too small
                            (bs',result) <- loop (biggerBuffer outSize) inBuff inBytesLeft
                            -- TODO: is this efficient enough?
                            return (bs `append` bs', result)
                    | err == eINVAL -> return (bs,Incomplete rest)
                    | otherwise     -> return (bs, Invalid rest)

partialIconv :: IConvT -> Int -> Ptr CString -> Ptr CSize -> IO (ByteString, Maybe Errno)
partialIconv cd outSize inBuff inBytesLeft =
    withForeignPtr cd $ \cd_p ->
    createAndTrim' outSize $ \outPtr ->
    with outPtr $ \outBuff ->
    with (toEnum outSize) $ \outBytesLeft -> do
        c_iconv cd_p inBuff inBytesLeft
                            (castPtr outBuff) outBytesLeft
        outLeft <- fmap fromEnum $ peek outBytesLeft
        inLeft <- peek inBytesLeft
        errno <- if inLeft > 0
                    then fmap Just getErrno
                    else return Nothing
        return (0,outSize - outLeft,errno)

