module System.Console.Haskeline.RunCommand (runCommandLoop) where

import System.Console.Haskeline.Command
import System.Console.Haskeline.Term
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Monads
import System.Console.Haskeline.Prefs
import System.Console.Haskeline.Key

import Control.Monad

runCommandLoop :: (MonadException m, CommandMonad m, MonadState Layout m)
    => TermOps -> String -> KeyCommand m InsertMode a -> m a
runCommandLoop tops prefix cmds = runTerm tops $ 
    RunTermType (withGetEvent tops $ runCommandLoop' tops prefix cmds)

runCommandLoop' :: forall t m a . (MonadTrans t, Term (t m), CommandMonad (t m),
        MonadState Layout m, MonadReader Prefs m)
        => TermOps -> String -> KeyCommand m InsertMode a -> t m Event -> t m a
runCommandLoop' tops prefix cmds getEvent = do
    let s = lineChars prefix emptyIM
    drawLine s
    loopKeys [] s (fmap ($ emptyIM) cmds)
  where 
    loopKeys :: [Key] -> LineChars -> KeyMap (CmdM m a) -> t m a
    loopKeys [] s processor = do -- no keys left, so read some more
        event <- handle (\(e::SomeException) -> moveToNextLine s
                                    >> throwIO e) getEvent
        case event of
                    ErrorEvent e -> moveToNextLine s >> throwIO e
                    WindowResize -> drawReposition tops s
                                    >> loopKeys [] s processor
                    KeyInput ks -> do
                        bound_ks <- mapM (lift . asks . lookupKeyBinding) ks
                        loopKeys (concat bound_ks) s processor
    loopKeys (k:ks) s processor = case lookupKM processor k of
                        Nothing -> actBell >> loopKeys [] s processor
                        Just (Consumed cmd) -> loopCmd ks s cmd
                        Just (NotConsumed cmd) -> loopCmd (k:ks) s cmd

    loopCmd :: [Key] -> LineChars -> CmdM m a -> t m a
    loopCmd ks s (GetKey next) = loopKeys ks s next
    loopCmd ks s (DoEffect e next) = do
                                        t <- drawEffect prefix s e
                                        loopCmd ks t next
    loopCmd ks s (CmdM next) = lift next >>= loopCmd ks s
    loopCmd _ s (Result x) = moveToNextLine s >> return x

drawEffect :: (MonadTrans t, Term (t m), MonadReader Prefs m)
    => String -> LineChars -> Effect -> t m LineChars
drawEffect prefix s (LineChange ch) = do
    let t = ch prefix
    drawLineDiff s t
    return t
drawEffect _ s ClearScreen = do
    clearLayout
    drawLine s
    return s
drawEffect _ s (PrintLines ls) = do
    when (s /= ([],[])) $ moveToNextLine s
    printLines ls
    drawLine s
    return s
drawEffect _ s RingBell = actBell >> return s

actBell :: (MonadTrans t, Term (t m), MonadReader Prefs m) => t m ()
actBell = do
    style <- lift $ asks bellStyle
    case style of
        NoBell -> return ()
        VisualBell -> ringBell False
        AudibleBell -> ringBell True

drawReposition :: (MonadTrans t, Term (t m), MonadState Layout m)
                    => TermOps -> LineChars -> t m ()
drawReposition tops s = do
    -- explicit lifts prevent the need for IncoherentInstances.
    oldLayout <- lift get
    newLayout <- liftIO $ getLayout tops
    when (oldLayout /= newLayout) $ do
        lift $ put newLayout
        reposition oldLayout s


