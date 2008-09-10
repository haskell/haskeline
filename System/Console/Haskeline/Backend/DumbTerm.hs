module System.Console.Haskeline.Backend.DumbTerm where

import System.Console.Haskeline.Backend.Posix
import System.Console.Haskeline.Term
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Monads as Monads
import System.Console.Haskeline.Command

import System.IO
import qualified System.IO.UTF8 as UTF8

-- TODO: 
---- Make this unicode-aware, too.
---- Put "<" and ">" at end of term if scrolls off.
---- Have a margin at the ends

data Window = Window {pos :: Int -- ^ # of visible chars to left of cursor
                }

initWindow :: Window
initWindow = Window {pos=0}

newtype DumbTerm m a = DumbTerm {unDumbTerm :: StateT Window m a}
                deriving (Monad,MonadIO, MonadState Window)

instance MonadReader Layout m => MonadReader Layout (DumbTerm m) where
    ask = lift ask
    local r = DumbTerm . local r . unDumbTerm

instance MonadException m => MonadException (DumbTerm m) where
    block = DumbTerm . block . unDumbTerm
    unblock = DumbTerm . unblock . unDumbTerm
    catch (DumbTerm f) g = DumbTerm $ Monads.catch f (unDumbTerm . g)

runDumbTerm :: (MonadLayout m, MonadException m) => RunTerm m
runDumbTerm = RunTerm {
    getLayout = getPosixLayout Nothing,
    withGetEvent = withPosixGetEvent Nothing,
    runTerm = evalStateT' initWindow . unDumbTerm,
    putStrTerm = UTF8.putStr
    }
    

instance MonadTrans DumbTerm where
    lift = DumbTerm . lift

instance MonadLayout m => Term (DumbTerm m) where
    withReposition _ = id
    drawLineDiff = drawLineDiff'
    
    printLines = mapM_ (\s -> printText (s ++ crlf))
    moveToNextLine = \_ -> printText crlf
    clearLayout = clearLayoutD
    ringBell True = printText "\a"
    ringBell False = return ()
      
printText :: MonadIO m => String -> m ()
printText str = liftIO $ UTF8.putStr str >> hFlush stdout

-- Things we can assume a dumb terminal knows how to do
cr,crlf :: String
crlf = "\r\n"
cr = "\r"

backs,spaces :: Int -> String
backs n = replicate n '\b'
spaces n = replicate n ' '


clearLayoutD :: MonadLayout m => DumbTerm m ()
clearLayoutD = do
    w <- maxWidth
    printText (cr ++ spaces w ++ cr)

-- Don't want to print in the last column, as that may wrap to the next line.
maxWidth :: MonadLayout m => DumbTerm m Int
maxWidth = asks (\lay -> width lay - 1)

drawLineDiff' :: (LineState s, LineState t, MonadLayout m)
                => String -> s -> t -> DumbTerm m ()
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
                _ -> let
                        extraLength = length xs1' + length ys1
                                    - length xs2' - length ys2
                     in printText $ backs (length xs1')
                        ++ xs2' ++ ys2' ++ clearDeadText extraLength
                        ++ backs (length ys2')

refitLine :: MonadLayout m => (String,String) -> DumbTerm m ()
refitLine (xs,ys) = do
    w <- maxWidth
    let xs' = dropFrames w xs
    let p = length xs'    
    put Window {pos=p}
    let ys' = take (w - p) ys
    let k = length ys'
    printText $ cr ++ xs' ++ ys'
        ++ spaces (w-k-p)
        ++ backs (w-p)
  where
    dropFrames w zs = case splitAt w zs of
                        (_,"") -> zs
                        (_,zs') -> dropFrames w zs'
    
clearDeadText :: Int -> String
clearDeadText n | n > 0 = spaces n ++ backs n
                | otherwise = ""
