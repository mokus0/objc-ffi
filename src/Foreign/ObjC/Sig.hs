module Foreign.ObjC.Sig where

import Foreign.ObjC.ObjCType

class ObjCSigType a where
    retTypeString  :: p a ->  String
    argTypeStrings :: p a -> [String]

instance ObjCType a => ObjCSigType (IO a) where
    retTypeString    = typeString . (const Nothing :: p (IO a) -> Maybe a)
    argTypeStrings _ = []

instance (ObjCType a, ObjCSigType b) => ObjCSigType (a -> b) where
    retTypeString = retTypeString . (const Nothing :: p (a -> b) -> Maybe b)
    argTypeStrings p
        = typeString     ((const Nothing :: p (a -> b) -> Maybe a) p)
        : argTypeStrings ((const Nothing :: p (a -> b) -> Maybe b) p)

sigTypeString :: ObjCSigType a => p a -> String
sigTypeString p = retTypeString p ++ concat (argTypeStrings p)