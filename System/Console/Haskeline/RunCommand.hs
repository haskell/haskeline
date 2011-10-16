module System.Console.Haskeline.RunCommand (runCommandLoop) where

import System.Console.Haskeline.Command
import System.Console.Haskeline.Term
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Monads
import System.Console.Haskeline.Prefs
import System.Console.Haskeline.Key

import Control.Monad

runCommandLoop :: (MonadException m, CommandMonad m, MonadState Layout m,
                    LineState s)
    => TermOps -> String -> KeyCommand m s a -> s -> m a
runCommandLoop tops prefix cmds initState = runTerm tops $ 
    RunTermType (withGetEvent tops
        $ runCommandLoop' tops (stringToGraphemes prefix) initState cmds)

runCommandLoop' :: forall t m s a . (MonadTrans t, Term (t m), CommandMonad (t m),
        MonadState Layout m, MonadReader Prefs m, LineState s)
        => TermOps -> Prefix -> s -> KeyCommand m s a -> t m Event -> t m a
runCommandLoop' tops prefix initState cmds getEvent = do
    let s = lineChars prefix initState
    drawLine s
    readMoreKeys s (fmap (liftM (\x -> (x,[])) . ($ initState)) cmds)
  where
    readMoreKeys :: LineChars -> KeyMap (CmdM m (a,[Key])) -> t m a
    readMoreKeys s next = do
        event <- handle (\(e::SomeException) -> moveToNextLine s
                                    >> throwIO e) getEvent
        case event of
                    ErrorEvent e -> moveToNextLine s >> throwIO e
                    WindowResize -> drawReposition tops s
                                        >> readMoreKeys s next
                    KeyInput ks -> do
                        bound_ks <- mapM (lift . asks . lookupKeyBinding) ks
                        loopCmd s $ applyKeysToMap (concat bound_ks) next

    loopCmd :: LineChars -> CmdM m (a,[Key]) -> t m a
    loopCmd s (GetKey next) = readMoreKeys s next
    -- If there are multiple consecutive LineChanges, only render the diff
    -- to the last one, and skip the rest. This greatly improves speed when
    -- a large amount of text is pasted in at once.
    loopCmd s (DoEffect (LineChange _)
                e@(DoEffect (LineChange _) _)) = loopCmd s e
    loopCmd s (DoEffect e next) = do
                                    t <- drawEffect prefix s e
                                    loopCmd t next
    loopCmd s (CmdM next) = lift next >>= loopCmd s
    loopCmd s (Result (x,ks)) = do
                                    liftIO (saveUnusedKeys tops ks)
                                    moveToNextLine s
                                    return x


drawEffect :: (MonadTrans t, Term (t m), MonadReader Prefs m)
    => Prefix -> LineChars -> Effect -> t m LineChars
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


---------------
-- Traverse through the tree of keybindings, using the given keys.
-- Remove as many GetKeys as possible.
-- Returns any unused keys (so that they can be applied at the next getInputLine).
applyKeysToMap :: Monad m => [Key] -> KeyMap (CmdM m (a,[Key]))
                                -> CmdM m (a,[Key])
applyKeysToMap [] next = GetKey next
applyKeysToMap (k:ks) next = case lookupKM next k of
    Nothing -> DoEffect RingBell $ GetKey next
    Just (Consumed cmd) -> applyKeysToCmd ks cmd
    Just (NotConsumed cmd) -> applyKeysToCmd (k:ks) cmd

applyKeysToCmd :: Monad m => [Key] -> CmdM m (a,[Key])
                                -> CmdM m (a,[Key])
applyKeysToCmd ks (GetKey next) = applyKeysToMap ks next
applyKeysToCmd ks (DoEffect e next) = DoEffect e (applyKeysToCmd ks next)
applyKeysToCmd ks (CmdM next) = CmdM $ liftM (applyKeysToCmd ks) next
applyKeysToCmd ks (Result (x,ys)) = Result (x,ys++ks) -- use in the next input line
