{-# LANGUAGE CPP #-}
import Distribution.System
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Setup

import Control.Monad(when)

main :: IO ()
main = defaultMainWithHooks myHooks

myHooks :: UserHooks
myHooks
    | buildOS == Windows    = simpleUserHooks
    | otherwise = simpleUserHooks {
            confHook = \genericDescript flags -> do
                        warnIfNotTerminfo flags
                        confHook simpleUserHooks genericDescript flags
            }

warnIfNotTerminfo flags = when (not (hasFlagSet flags (FlagName "terminfo")))
    $ mapM_ putStrLn
    [ "*** Warning: running on POSIX but not building the terminfo backend. ***"
    , "You may need to install the terminfo package manually, e.g. with"
    , "\"cabal install terminfo\"; or, use \"-fterminfo\" when configuring or"
    , "installing this package."
    ,""
    ]

hasFlagSet :: ConfigFlags -> FlagName -> Bool
hasFlagSet cflags flag = Just True == lookup flag (configConfigurationsFlags cflags)
