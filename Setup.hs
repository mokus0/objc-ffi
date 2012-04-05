import Distribution.Simple
import Distribution.PackageDescription
import System.Info
import System.GNUstep.Config

main = do
    defaultMainWithHooks simpleUserHooks
        { preConf  = \_ _ -> findGNUstep
        , preBuild = \_ _ -> findGNUstep
        }

findGNUstep
    | os == "darwin"    = return (Nothing, [])
    | otherwise         = do
        lib     <- variable "GNUSTEP_SYSTEM_LIBRARIES"
        include <- variable "GNUSTEP_SYSTEM_HEADERS"
        
        let buildInfo = emptyBuildInfo
                { extraLibDirs  = [lib]
                , includeDirs   = [include]
                }
        
        return (Just buildInfo, [])
