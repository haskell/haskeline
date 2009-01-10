{- |
A Unicode-aware module for interacting with files.  We just need enough to support
filename completion.  In particular, these functions will silently handle all errors
(for example, file does not exist)
-}
module System.Console.Haskeline.Directory(
                    getDirectoryContents,
                    doesDirectoryExist,
                    getHomeDirectory
                    ) where

#ifdef MINGW

import Foreign
import Foreign.C
import System.Win32.Types
import Data.Bits

#include <windows.h>
#include <Shlobj.h>

foreign import stdcall "FindFirstFileW" c_FindFirstFile
            :: LPCTSTR -> Ptr () -> IO HANDLE

foreign import stdcall "FindNextFileW" c_FindNextFile
            :: HANDLE -> Ptr () -> IO Bool

foreign import stdcall "FindClose" c_FindClose :: HANDLE -> IO BOOL

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents fp = allocaBytes (#size WIN32_FIND_DATA) $ \findP ->
    withCWString (fp ++ "\\*") $ \t_arr -> do
        h <- c_FindFirstFile t_arr findP
        if h == iNVALID_HANDLE_VALUE
            then return []
            else loop h findP
  where
    loop h findP = do
        f <- peekFileName findP
        isNext <- c_FindNextFile h findP
        if isNext
            then do {fs <- loop h findP; return (f:fs)}
            else c_FindClose h >> return [f]
    peekFileName = peekCWString . (#ptr WIN32_FIND_DATA, cFileName)

foreign import stdcall "GetFileAttributesW" c_GetFileAttributes
            :: LPCTSTR -> IO DWORD

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist file = do
    attrs <- withCWString file c_GetFileAttributes
    return $ attrs /= (#const INVALID_FILE_ATTRIBUTES)
            && (attrs .&. (#const FILE_ATTRIBUTE_DIRECTORY)) /= 0

type HRESULT = #type HRESULT

foreign import stdcall "SHGetFolderPathW" c_SHGetFolderPath
    :: Ptr () -> CInt -> HANDLE -> DWORD -> LPTSTR -> IO HRESULT

getHomeDirectory :: IO FilePath
getHomeDirectory = allocaBytes ((#const MAX_PATH) * (#size TCHAR)) $ \pathPtr -> do
    result <- c_SHGetFolderPath nullPtr (#const CSIDL_PROFILE) nullPtr 0 pathPtr
    if result /= (#const S_OK)
        then return ""
        else peekCWString pathPtr

#else

import Data.ByteString.Char8 (pack, unpack)
import qualified System.Directory as D
import Control.Exception.Extensible
import System.Console.Haskeline.Backend.IConv

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents path = do
    codeset <- getCodeset
    encoder <- openEncoder codeset
    decoder <- openDecoder codeset
    dirEnc <- fmap unpack (encoder path)
    filesEnc <- handle (\(_::IOException) -> return [])
                    $ D.getDirectoryContents dirEnc
    mapM (decoder . pack) filesEnc

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist file = do
    codeset <- getCodeset
    encoder <- openEncoder codeset
    encoder file >>= D.doesDirectoryExist . unpack

getHomeDirectory :: IO FilePath
getHomeDirectory = do
    codeset <- getCodeset
    decoder <- openDecoder codeset
    handle (\(_::IOException) -> return "")
        $ D.getHomeDirectory >>= decoder . pack
#endif
