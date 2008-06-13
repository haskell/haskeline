#!/usr/local/bin/runhaskell

\begin{code}
import Distribution.Simple
import Distribution.Verbosity
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Simple.GHC as GHC
import Distribution.Simple.PreProcess

main = defaultMainWithHooks simpleUserHooks {runTests = buildTestProg}

buildTestProg :: [String] -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
buildTestProg _ _ pkgDescr lbi = do
    let newPkg = withTest pkgDescr
    preprocessSources newPkg lbi False normal [("hsc",ppHsc2hs)]
    GHC.build (withTest pkgDescr) lbi normal

withTest :: PackageDescription -> PackageDescription
withTest pkg@PackageDescription {library = Just lib} 
                = pkg {executables = [testProg],
                        library = Nothing}
    where
        testProg = Executable {exeName = "Test",
                                modulePath = "examples/Test.hs",
                                buildInfo = alterBuildInfo (libBuildInfo lib)}

alterBuildInfo :: BuildInfo -> BuildInfo
alterBuildInfo = id

\end{code}
