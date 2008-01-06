module System.Console.HaskLine.WindowSize(WindowSize(..),
                 getWindowSize) where

import Foreign
import Foreign.C.Types

#include <sys/ioctl.h>

data WindowSize = WindowSize {winRows, winCols :: CUShort}
                deriving Show

instance Storable WindowSize where
    sizeOf _ = (#size struct winsize)
    alignment _ = (#size struct winsize) -- ????
    peek p = do 
                r <- (#peek struct winsize,ws_row) p
                c <- (#peek struct winsize,ws_col) p
                return WindowSize {winRows = r, winCols = c}

tiocgwinsz :: CULong
tiocgwinsz = #const TIOCGWINSZ

foreign import ccall ioctl :: CInt -> CULong -> Ptr a -> IO ()


getWindowSize :: IO WindowSize
getWindowSize = alloca $ \ws -> do
                            ioctl 1 tiocgwinsz ws
                            peek ws

-- TODO: other ways of getting it:
-- env vars ROWS/COLUMNS
-- terminfo capabilities
