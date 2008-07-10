module System.Console.Haskeline.Backend.Win32(
                Draw(),
                win32Term
                )where


import System.IO
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Foreign.Marshal.Utils
import System.Win32.Types
import Graphics.Win32.Misc(getStdHandle, sTD_INPUT_HANDLE, sTD_OUTPUT_HANDLE)
import Data.List(intercalate)
import Control.Concurrent
import Control.Concurrent.STM

import System.Console.Haskeline.Command
import System.Console.Haskeline.Monads
import System.Console.Haskeline.LineState
import System.Console.Haskeline.InputT
import System.Console.Haskeline.Term

#include "win_console.h"

foreign import stdcall "windows.h ReadConsoleInputW" c_ReadConsoleInput
    :: HANDLE -> Ptr () -> DWORD -> Ptr DWORD -> IO Bool
    
foreign import stdcall "windows.h WaitForSingleObject" c_WaitForSingleObject
    :: HANDLE -> DWORD -> IO DWORD

getEvent :: HANDLE -> TChan Event -> IO Event
getEvent h = keyEventLoop readKeyEvents
  where
    waitTime = 200 -- milliseconds
    readKeyEvents eventChan = do
        yield -- since the foreign call to WaitForSingleObject blocks the killThread
        ret <- c_WaitForSingleObject h waitTime
        if ret /= (#const WAIT_OBJECT_0)
            then readKeyEvents eventChan
            else do
                e <- readEvent h
                case eventToKey e of
	            Just k -> atomically $ writeTChan eventChan (KeyInput k)
    	            Nothing -> readKeyEvents eventChan

            
eventToKey :: InputEvent -> Maybe Key
eventToKey KeyEvent {keyDown = True, unicodeChar = c, virtualKeyCode = vc}
    | c /= '\NUL' = Just (KeyChar c)
    | otherwise = keyFromCode vc -- special character; see below.
eventToKey _ = Nothing

keyFromCode :: WORD -> Maybe Key
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
            -- I cant figure out how the user generates them.
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
    char :: CWchar <- (#peek KEY_EVENT_RECORD, uChar) p
    state <- (#peek KEY_EVENT_RECORD, dwControlKeyState) p
    return KeyEvent {keyDown = kDown',
                            repeatCount = repeat',
                            virtualKeyCode = keyCode,
                            virtualScanCode = scanCode,
                            unicodeChar = toEnum (fromEnum char),
                            controlKeyState = state}

data Coord = Coord {coordX, coordY :: Int}
                deriving Show
                
instance Storable Coord where
    sizeOf _ = (#size COORD)
    alignment = undefined -- ???
    peek p = do
        x :: CShort <- (#peek COORD, X) p
        y :: CShort <- (#peek COORD, Y) p
        return Coord {coordX = fromEnum x, coordY = fromEnum y}
    poke p c = do
        (#poke COORD, X) p (toEnum (coordX c) :: CShort)
        (#poke COORD, Y) p (toEnum (coordY c) :: CShort)
                
                            
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

withScreenBufferInfo :: (Ptr () -> IO a) -> HANDLE -> IO a
withScreenBufferInfo f h = allocaBytes (#size CONSOLE_SCREEN_BUFFER_INFO)
                                $ \infoPtr -> do
        failIfFalse_ "GetConsoleScreenBufferInfo"
            $ c_GetScreenBufferInfo h infoPtr
        f infoPtr


getDisplayWindow :: HANDLE -> IO (Coord,Coord)
getDisplayWindow = withScreenBufferInfo $ \p -> do
    let windowPtr = (#ptr CONSOLE_SCREEN_BUFFER_INFO, srWindow) p
    left <- (#peek SMALL_RECT, Left) windowPtr
    top <- (#peek SMALL_RECT, Top) windowPtr
    right <- (#peek SMALL_RECT, Right) windowPtr
    bottom <- (#peek SMALL_RECT, Bottom) windowPtr
    return (Coord (cvt left) (cvt top), Coord (cvt right) (cvt bottom))
  where
    cvt :: CShort -> Int
    cvt = fromEnum

----------------------------
-- Drawing

newtype Draw m a = Draw {runDraw :: m a}
    deriving (Monad,MonadIO,MonadException)

instance MonadTrans Draw where
    lift = Draw

instance MonadReader Layout m => MonadReader Layout (Draw m) where
    ask = lift ask
    local r = Draw . local r . runDraw

getInputHandle, getOutputHandle :: MonadIO m => m HANDLE
getInputHandle = liftIO $ getStdHandle sTD_INPUT_HANDLE
getOutputHandle = liftIO $ getStdHandle sTD_OUTPUT_HANDLE

getDisplaySize :: IO Layout
getDisplaySize = do
    h <- getOutputHandle
    (topLeft,bottomRight) <- getDisplayWindow h
    return Layout {width = coordX bottomRight - coordX topLeft+1, 
                    height = coordY bottomRight - coordY topLeft+1 }
    
getPos :: MonadIO m => Draw m Coord
getPos = getOutputHandle >>= liftIO . getPosition
    
setPos :: MonadIO m => Coord -> Draw m ()
setPos c = do
    h <- getOutputHandle
    liftIO (setPosition h c)

-- TODO: is it bad to be using putStr here?
-- 
-- NOTE: we need to call hflush explicitly here, so that getPos/setPos are synced
-- with the character output.
printText :: MonadIO m => String -> Draw m ()
printText txt = liftIO (putStr txt >> hFlush stdout)
    
printAfter :: MonadIO m => String -> Draw (InputCmdT m) ()
printAfter str = do
    p <- getPos
    printText str
    setPos p
    
drawLineDiffWin :: (LineState s, LineState t, MonadIO m)
                        => String -> s -> t -> Draw (InputCmdT m) ()
drawLineDiffWin prefix s1 s2 = let
    xs1 = beforeCursor prefix s1
    ys1 = afterCursor s1
    xs2 = beforeCursor prefix s2
    ys2 = afterCursor s2
    in case matchInit xs1 xs2 of
        ([],[])     | ys1 == ys2            -> return ()
        (xs1',[])   | xs1' ++ ys1 == ys2    -> movePos $ negate $ length xs1'
        ([],xs2')   | ys1 == xs2' ++ ys2    -> movePos $ length xs2'
        (xs1',xs2')                         -> do
            movePos (negate $ length xs1')
            let m = length xs1' + length ys1 - (length xs2' + length ys2)
            let deadText = replicate m ' '
            printText xs2'
            printAfter (ys2 ++ deadText)

movePos :: MonadIO m => Int -> Draw (InputCmdT m) ()
movePos n = do
    Coord {coordX = x, coordY = y} <- getPos
    w <- asks width
    let (h,x') = divMod (x+n) w
    setPos Coord {coordX = x', coordY = y+h}

crlf :: String
crlf = "\r\n"

instance MonadIO m => Term (Draw (InputCmdT m)) where
    drawLineDiff = drawLineDiffWin
    withReposition _ = id -- TODO

    printLines [] = return ()
    printLines ls = printText $ intercalate crlf ls ++ crlf
    
    clearLayout = do
        lay <- ask
        setPos (Coord 0 0)
        printText (replicate (width lay * height lay) ' ')
        setPos (Coord 0 0)
    
    moveToNextLine s = do
        movePos (lengthToEnd s)
        printText "\r\n" -- make the console take care of creating a new line
    
    ringBell _ = return () -- TODO

win32Term :: MonadException m => RunTerm (InputCmdT m)
win32Term = RunTerm {
    getLayout = getDisplaySize,
    runTerm = runDraw,
    withGetEvent = \useSigINT f -> do 
        h <- getInputHandle
	eventChan <- liftIO $ newTChanIO
        withCtrlCHandler useSigINT eventChan
		$ f $ liftIO $ getEvent h eventChan
    }

type Handler = DWORD -> IO BOOL

foreign import ccall "wrapper" wrapHandler :: Handler -> IO (FunPtr Handler)

foreign import stdcall "windows.h SetConsoleCtrlHandler" c_SetConsoleCtrlHandler
    :: FunPtr Handler -> BOOL -> IO BOOL

-- sets the tv to True when ctrl-c is pressed.
withCtrlCHandler :: MonadException m => Bool -> TChan Event -> m a -> m a
withCtrlCHandler False _ f = f
withCtrlCHandler True eventChan f = bracket (liftIO $ do
                                    fp <- wrapHandler handler
                                    c_SetConsoleCtrlHandler fp True
                                    return fp)
                                (\fp -> liftIO $ c_SetConsoleCtrlHandler fp False)
                                (const f)
  where
    handler (#const CTRL_C_EVENT) = do
        atomically $ writeTChan eventChan SigInt
        return True
    handler _ = return False
