import Distribution.System
import Distribution.Verbosity
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils

import System.IO
import System.Exit
import System.Directory
import Control.Exception.Extensible

-- TODO: it's a hack that we use the autoconfUserHooks when we're not actually
-- using autoconf...
main :: IO ()
main = defaultMainWithHooks myHooks

myHooks :: UserHooks
myHooks
    | buildOS == Windows    = simpleUserHooks
    | otherwise = autoconfUserHooks {
            postConf = \args flags pkgDescr lbi -> do
                            let Just lib = library pkgDescr
                            let bi = libBuildInfo lib
                            bi' <- maybeSetLibiconv flags bi
                                                        (withPrograms lbi)
                            let pkgDescr' = pkgDescr {
                                                library = Just lib {
                                                    libBuildInfo = bi'}}
                            postConf simpleUserHooks args flags pkgDescr' lbi
            }

-- Test whether compiling a c program that links against libiconv needs -liconv.
maybeSetLibiconv :: ConfigFlags -> BuildInfo -> ProgramConfiguration -> IO BuildInfo
maybeSetLibiconv flags bi progConf = do
    let verb = fromFlag (configVerbosity flags)
    if hasFlagSet flags (FlagName "libiconv")
        then do
            putStrLn "Using -liconv."
            writeBuildInfo "iconv"
            return bi
        else do
    putStr "checking whether to use -liconv... "
    hFlush stdout
    worksWithout <- tryCompile iconv_prog bi progConf verb
    if worksWithout
        then do
            putStrLn "not needed."
            writeBuildInfo ""
            return bi
        else do
    let newBI = addIconv bi
    worksWith <- tryCompile iconv_prog newBI progConf verb
    if worksWith
        then do
            putStrLn "using -liconv."
            writeBuildInfo "iconv"
            return newBI
        else error "Unable to link against the iconv library."
  where
    -- Cabal (at least 1.6.0.1) won't parse an empty buildinfo file.
    writeBuildInfo libs = writeFile "haskeline.buildinfo"
                            $ unlines ["extra-libraries: " ++ libs]

hasFlagSet :: ConfigFlags -> FlagName -> Bool
hasFlagSet cflags flag = Just True == lookup flag (configConfigurationsFlags cflags)

tryCompile :: String -> BuildInfo -> ProgramConfiguration -> Verbosity -> IO Bool
tryCompile program bi progConf verb = handle processExit $ handle processException $ do
    tempDir <- getTemporaryDirectory
    withTempFile tempDir ".c" $ \fname h -> do
        hPutStr h program
        hClose h
        -- TODO take verbosity from the args.
        rawSystemProgramStdoutConf verb gccProgram progConf (fname : args)
        return True
  where
    processException :: IOException -> IO Bool
    processException e = return False
    processExit = return . (==ExitSuccess)
    args = concat [ []
                  , ccOptions bi
                  , cppOptions bi
                  , ldOptions bi
                  -- --extra-include-dirs and --extra-lib-dirs are included
                  -- in the below fields.
                  , map ("-I" ++) (includeDirs bi)
                  , map ("-L" ++) (extraLibDirs bi)
                  , map ("-l" ++) (extraLibs bi)
                  ]

addIconv :: BuildInfo -> BuildInfo
addIconv bi = bi {extraLibs = "iconv" : extraLibs bi}

iconv_prog :: String
iconv_prog = unlines $
    [ "#include <iconv.h>"
    , "int main(void) {"
    , "    iconv_t t = iconv_open(\"UTF-8\", \"UTF-8\");"
    , "    return 0;"
    , "}"
    ]
    
