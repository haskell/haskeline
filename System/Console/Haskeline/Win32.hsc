module System.Console.Haskeline.Win32(
                HANDLE,
                Coord(..),
                readKey,
                getConsoleSize,
                getPosition,
                setPosition
                )where


import System.IO
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Foreign.Marshal.Utils
import System.Win32.Types
import Graphics.Win32.Misc

import System.Console.Haskeline.Command

#include "win_console.h"

foreign import stdcall "windows.h GetConsoleMode" c_GetConsoleMode 
    :: HANDLE -> Ptr DWORD -> IO Bool

foreign import stdcall "windows.h SetConsoleMode" c_SetConsoleMode
    :: HANDLE -> DWORD -> IO Bool
    
getConsoleMode :: HANDLE -> IO DWORD
getConsoleMode h = alloca $ \modePtr -> do
    failIfFalse_ "GetConsoleMode" $ c_GetConsoleMode h modePtr 
    peek modePtr
    
setConsoleMode :: HANDLE -> DWORD -> IO ()
setConsoleMode h m = failIfFalse_ "SetConsoleMode" $ c_SetConsoleMode h m

foreign import stdcall "windows.h ReadConsoleInputA" c_ReadConsoleInput
    :: HANDLE -> Ptr () -> DWORD -> Ptr DWORD -> IO Bool
    
readKey :: HANDLE -> IO Key
readKey h = do
    e <- readEvent h
    case e of
        KeyEvent {keyDown = True, unicodeChar = c, virtualKeyCode = vc}
            | c /= '\NUL'                   -> return (KeyChar c)
            | Just k <- keyFromCode vc      -> return k
        _ -> readKey h

keyFromCode (#const VK_BACK) = Just Backspace
keyFromCode (#const VK_LEFT) = Just KeyLeft
keyFromCode (#const VK_RIGHT) = Just KeyRight
keyFromCode (#const VK_UP) = Just KeyUp
keyFromCode (#const VK_DOWN) = Just KeyDown
keyFromCode (#const VK_DELETE) = Just DeleteForward
-- TODO: KeyMeta (option-x), KillLine
keyFromCode _ = Nothing
    
data InputEvent = KeyEvent {keyDown :: BOOL,
                          repeatCount :: WORD,
                          virtualKeyCode :: WORD,
                          virtualScanCode :: WORD,
                          unicodeChar :: Char,
                          controlKeyState :: DWORD}
            -- TODO: WINDOW_BUFFER_SIZE_RECORD
            -- I can't figure out how the user generates them.
           | OtherEvent
                        deriving Show

readEvent :: HANDLE -> IO InputEvent
readEvent h = allocaBytes (#size INPUT_RECORD) $ \pRecord -> 
                        alloca $ \numEventsPtr -> do
    failIfFalse_ "ReadConsoleInput" 
        $ c_ReadConsoleInput h pRecord 1 numEventsPtr
    -- useful? numEvents <- peek numEventsPtr
    eventType :: WORD <- (#peek INPUT_RECORD, EventType) pRecord
    let eventPtr = (#ptr INPUT_RECORD, Event) pRecord
    case eventType of
        (#const KEY_EVENT) -> getKeyEvent eventPtr
        _ -> return OtherEvent
        
getKeyEvent :: Ptr () -> IO InputEvent
getKeyEvent p = do
    kDown' <- (#peek KEY_EVENT_RECORD, bKeyDown) p
    repeat' <- (#peek KEY_EVENT_RECORD, wRepeatCount) p
    keyCode <- (#peek KEY_EVENT_RECORD, wVirtualKeyCode) p
    scanCode <- (#peek KEY_EVENT_RECORD, wVirtualScanCode) p
    char :: CChar <- (#peek KEY_EVENT_RECORD, uChar) p -- TODO: unicode?
    state <- (#peek KEY_EVENT_RECORD, dwControlKeyState) p
    return KeyEvent {keyDown = kDown',
                            repeatCount = repeat',
                            virtualKeyCode = keyCode,
                            virtualScanCode = scanCode,
                            unicodeChar = toEnum $ fromEnum char,
                            controlKeyState = state}

-- NOTE: may be good to make COORD Storable, since used in multiple places.
data Coord = Coord {coordX, coordY :: CShort}
                deriving Show
                
instance Storable Coord where
    sizeOf _ = (#size COORD)
    alignment = undefined -- ???
    peek p = do
        x <- (#peek COORD, X) p
        y <- (#peek COORD, Y) p
        return Coord {coordX = x, coordY = y}
    poke p c = do
        (#poke COORD, X) p (coordX c)
        (#poke COORD, Y) p (coordY c)
                
                            
foreign import ccall "SetPosition"
    c_SetPosition :: HANDLE -> Ptr Coord -> IO Bool
    
setPosition :: HANDLE -> Coord -> IO ()
setPosition h c = with c $ failIfFalse_ "SetConsoleCursorPosition" 
                    . c_SetPosition h
                    
foreign import stdcall "windows.h GetConsoleScreenBufferInfo"
    c_GetScreenBufferInfo :: HANDLE -> Ptr () -> IO Bool
    
getPosition :: HANDLE -> IO Coord
getPosition = withScreenBufferInfo $ 
    (#peek CONSOLE_SCREEN_BUFFER_INFO, dwCursorPosition)

getConsoleSize :: HANDLE -> IO Coord
getConsoleSize = withScreenBufferInfo $
    (#peek CONSOLE_SCREEN_BUFFER_INFO, dwSize)
    
withScreenBufferInfo :: (Ptr () -> IO a) -> HANDLE -> IO a
withScreenBufferInfo f h = allocaBytes (#size CONSOLE_SCREEN_BUFFER_INFO)
                                $ \infoPtr -> do
        failIfFalse_ "GetConsoleScreenBufferInfo"
            $ c_GetScreenBufferInfo h infoPtr
        f infoPtr
        
