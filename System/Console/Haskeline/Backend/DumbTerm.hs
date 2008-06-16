module System.Console.Haskeline.Backend.DumbTerm where

import System.Console.Haskeline.Backend.Posix
import System.Console.Haskeline.InputT
import System.Console.Haskeline.Term
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Monads
import System.Console.Haskeline.Command

import System.IO

-- TODO: 
---- Make this unicode-aware, too.
---- Put "<" and ">" at end of term if scrolls off.
---- Have a margin at the ends

data Window = Window {pos :: Int -- ^ # of visible chars to left of cursor
                }

initWindow :: Window
initWindow = Window {pos=0}

newtype DumbTerm m a = DumbTerm (StateT Window m a) -- keep track of window
                deriving (Monad,MonadIO, MonadState Window)

runDumbTerm :: MonadIO m => RunTerm DumbTerm m
runDumbTerm = RunTerm {
    getLayout = getPosixLayout,
    withGetEvent = withPosixGetEvent Nothing,
    runTerm = \(DumbTerm f) -> evalStateT initWindow f
    }
    

instance MonadTrans DumbTerm where
    lift = DumbTerm . lift
    lift2 f (DumbTerm m) = DumbTerm (lift2 f m)

instance MonadIO m => Term (DumbTerm (InputCmdT m)) where
    withReposition _ = id
    drawLineDiff = drawLineDiff'
    
    printLines = mapM_ (\s -> printText (s ++ crlf))
    moveToNextLine = \_ -> printText crlf
    clearLayout = clearLayoutD
      
printText :: MonadIO m => String -> m ()
printText str = liftIO $ putStr str >> hFlush stdout

-- Things we can assume a dumb terminal knows how to do
cr,crlf :: String
crlf = "\r\n"
cr = "\r"

backs,spaces :: Int -> String
backs n = replicate n '\b'
spaces n = replicate n ' '


clearLayoutD :: MonadIO m => DumbTerm (InputCmdT m) ()
clearLayoutD = do
    w <- maxWidth
    printText (cr ++ spaces w ++ cr)

-- Don't want to print in the last column, as that may wrap to the next line.
maxWidth :: Monad m => DumbTerm (InputCmdT m) Int
maxWidth = asks (\lay -> width lay - 1)

drawLineDiff' :: (LineState s, LineState t, MonadIO m)
                => String -> s -> t -> DumbTerm (InputCmdT m) ()
drawLineDiff' prefix s1 s2 = do
    let xs1 = beforeCursor prefix s1
    let ys1 = afterCursor s1
    let xs2 = beforeCursor prefix s2
    let ys2 = afterCursor s2
    Window {pos=p} <- get
    w <- maxWidth
    let (xs1',xs2') = matchInit xs1 xs2
    let newP = p + length xs2' - length xs1'
    let ys2' = take (w-newP) ys2
    if length xs1' > p  || newP >= w
        then refitLine (xs2,ys2)
        else do -- we haven't moved outside the margins
            put Window {pos=newP}
            case (xs1',xs2') of
                ([],[]) | ys1 == ys2    -> return () -- no change
                (_,[]) | xs1' ++ ys1 == ys2 -> -- moved left
                    printText $ backs (length xs1')
                ([],_) | ys1 == xs2' ++ ys2 -> -- moved right
                    printText xs2'
                _ -> printText $ backs (length xs1')
                        ++ xs2' ++ ys2' ++ backs (length ys2')

refitLine :: MonadIO m => (String,String) -> DumbTerm (InputCmdT m) ()
refitLine (xs,ys) = do
    w <- maxWidth
    let xs' = dropFrames w xs
    let p = length xs'    
    put Window {pos=p}
    let ys' = take (w - p) ys
    let k = length ys'
    printText $ [cr] ++ xs' ++ ys'
        ++ spaces (w-k-p)
        ++ backs (w-p)
  where
    dropFrames w zs = case splitAt w zs of
                        (_,"") -> zs
                        (_,zs') -> dropFrames w zs'
    
