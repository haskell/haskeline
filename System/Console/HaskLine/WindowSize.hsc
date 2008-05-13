module System.Console.HaskLine.WindowSize(WindowSize(..),
                 getWindowSize) where

import Foreign
import Foreign.C.Types
import Control.Monad(liftM2)

#include <sys/ioctl.h>

data WindowSize = WindowSize {winRows, winCols :: CUShort}
                deriving Show

foreign import ccall ioctl :: CInt -> CULong -> Ptr a -> IO ()

getWindowSize :: IO WindowSize
getWindowSize = allocaBytes (#size struct winsize) $ \ws -> do
                            ioctl 1 (#const TIOCGWINSZ) ws
                            liftM2 WindowSize ((#peek struct winsize,ws_row) ws)
                                              ((#peek struct winsize,ws_col) ws)

-- TODO: other ways of getting it:
-- env vars ROWS/COLUMNS
-- terminfo capabilities
