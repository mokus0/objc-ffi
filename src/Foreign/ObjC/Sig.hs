module Foreign.ObjC.Sig where

import Foreign.LibFFI.Experimental
import Foreign.ObjC.ObjCType

class SigType a => ObjCSigType a where
    retTypeString  :: p a ->  String
    argTypeStrings :: p a -> [String]

instance ObjCRet a => ObjCSigType (IO a) where
    retTypeString    = typeString . (const Nothing :: p (IO a) -> Maybe a)
    argTypeStrings _ = []

instance (ObjCArg a, ObjCSigType b) => ObjCSigType (a -> b) where
    retTypeString = retTypeString . (const Nothing :: p (a -> b) -> Maybe b)
    argTypeStrings p
        = typeString     ((const Nothing :: p (a -> b) -> Maybe a) p)
        : argTypeStrings ((const Nothing :: p (a -> b) -> Maybe b) p)

sigTypeString :: ObjCSigType a => p a -> String
sigTypeString p = retTypeString p ++ concat (argTypeStrings p)