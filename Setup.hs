import Distribution.Simple
import Distribution.PackageDescription
import System.Info
import System.GNUstep.Config

main = do
    -- cabal warns about deprecation here, but simpleUserHooks just doesn't
    -- work.  It appears to just discard the "HookedBuildInfo" returned by
    -- preconf, so we use the old-school .buildinfo file route.
    defaultMainWithHooks defaultUserHooks
        { preConf  = \_ _ -> findGNUstep }

findGNUstep
    | os == "darwin"    = return (Nothing, [])
    | otherwise         = do
        lib     <- variable "GNUSTEP_SYSTEM_LIBRARIES"
        include <- variable "GNUSTEP_SYSTEM_HEADERS"
        
        writeFile "objc-ffi.buildinfo" $ unlines
            [ "extra-lib-dirs: " ++ lib
            , "include-dirs:   " ++ include
            ]
        
        return (Nothing, [])
