module HaskelineExport where

import System.Console.Haskeline
import System.Console.Haskeline.IO

import Foreign
import Foreign.C.String

foreign export ccall initialize_haskeline :: IO (StablePtr HaskelineData)

foreign export ccall close_haskeline :: StablePtr HaskelineData -> IO ()

foreign export ccall cancel_haskeline:: StablePtr HaskelineData -> IO ()

foreign export ccall get_input_line :: StablePtr HaskelineData -> CString
                                        -> IO CString

-- TODO: allocate string results with the malloc from stdlib 
-- so that c code can free it.

initialize_haskeline = do
    hd <- initHaskeline defaultSettings
    newStablePtr hd


close_haskeline sptr = do
    hd <- deRefStablePtr sptr
    closeHaskeline hd
    freeStablePtr sptr

cancel_haskeline sptr = do
    hd <- deRefStablePtr sptr
    cancelHaskeline hd
    freeStablePtr sptr


get_input_line sptr c_prefix = do
    hd <- deRefStablePtr sptr
    prefix <- peekCString c_prefix
    result <- askHaskeline hd (getInputLine prefix)
    case result of
        Nothing -> return nullPtr
        Just str -> newCString str

