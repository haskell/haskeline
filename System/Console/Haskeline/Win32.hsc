module System.Console.Haskeline.Win32(
                Draw(),
                runDraw,
                getLayout,
                drawEffect,
                moveToNextLine,
                drawLine,
                withReposition,
                withGetEvent
                )where


import System.IO
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Foreign.Marshal.Utils
import System.Win32.Types
import Graphics.Win32.Misc(getStdHandle, sTD_INPUT_HANDLE, sTD_OUTPUT_HANDLE)
import Control.Monad(when,liftM)
import Data.List(intercalate)


import System.Console.Haskeline.Command
import System.Console.Haskeline.Monads
import System.Console.Haskeline.LineState
import System.Console.Haskeline.InputT

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
            -- first, some special cases to make this look more unix-y to the KeyMaps.
            | c == '\b'                     -> return (Backspace)
            | c == '\r'                     -> return (KeyChar '\n')
            -- regular character; just return it.
            | c /= '\NUL'                   -> return (KeyChar c)
            -- special character; see below.
            | Just k <- keyFromCode vc      -> return k
        -- If the key is not recognized, ignore it and try again.
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

getConsoleSize :: HANDLE -> IO Coord
getConsoleSize = withScreenBufferInfo $
    (#peek CONSOLE_SCREEN_BUFFER_INFO, dwSize)

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
    deriving (Monad,MonadIO)

instance MonadTrans Draw where
    lift = Draw
    lift2 f = Draw . f . runDraw

getInputHandle, getOutputHandle :: MonadIO m => m HANDLE
getInputHandle = liftIO $ getStdHandle sTD_INPUT_HANDLE
getOutputHandle = liftIO $ getStdHandle sTD_OUTPUT_HANDLE

getLayout :: IO Layout
getLayout = do
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

moveToNextLine :: (MonadIO m, LineState s) => s -> Draw (InputCmdT m) ()
moveToNextLine s = do
    movePos (lengthToEnd s)
    printText "\r\n" -- make the console take care of creating a new line
    
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
    
drawLine :: (MonadIO m, LineState s) => String -> s -> Draw (InputCmdT m) ()
drawLine prefix s = do
    printText (beforeCursor prefix s)
    printAfter (afterCursor s)
    
diffLinesBreaking :: (LineState s, LineState t, MonadIO m)
                        => String -> s -> t -> Draw (InputCmdT m) ()
diffLinesBreaking prefix s1 s2 = let
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

-- todo: Dupe of Draw.hs
matchInit :: Eq a => [a] -> [a] -> ([a],[a])
matchInit (x:xs) (y:ys)  | x == y = matchInit xs ys
matchInit xs ys = (xs,ys)

movePos :: MonadIO m => Int -> Draw (InputCmdT m) ()
movePos n = do
    Coord {coordX = x, coordY = y} <- getPos
    w <- asks width
    let (h,x') = divMod (x+n) w
    setPos Coord {coordX = x', coordY = y+h}

crlf = "\r\n"

drawEffect :: (LineState s, LineState t, MonadIO m) 
    => String -> s -> Effect t -> Draw (InputCmdT m) ()
drawEffect prefix s (Change t) = do
    diffLinesBreaking prefix s t
drawEffect prefix s (PrintLines ls t overwrite) = do
    if overwrite
        then diffLinesBreaking prefix s Cleared 
        else moveToNextLine s
    printText $ intercalate crlf ls
    when (not (null ls)) $ printText crlf
    drawLine prefix t
drawEffect prefix s (Redraw shouldClear t) = do
    if shouldClear
        then clearScreenAndRedraw prefix t
        else redrawLine prefix t
  where
    redrawLine prefix t = do
        movePos $ negate $ length $ beforeCursor prefix s
        drawLine prefix t
    -- TODO: this scrolls all the way to the top; is that right?
    -- also: should I be using FillConsoleOutputCharacter?
    clearScreenAndRedraw prefix t = do
        lay <- liftIO getLayout
        setPos (Coord 0 0)
        printText (replicate (width lay * height lay) ' ')
        setPos (Coord 0 0)
        drawLine prefix t


-- TODO: implement
withReposition :: Monad m => Layout -> Draw (InputCmdT m) a -> Draw (InputCmdT m) a
withReposition _ = id

-- TODO: Use GHC.ConsoleHandler.installHandler for ctrl-c events
withGetEvent :: MonadIO m => Bool -> (m Event -> m a) -> m a
withGetEvent _ f = do
    h <- getInputHandle
    f $ liftIO $ liftM KeyInput $ readKey h
