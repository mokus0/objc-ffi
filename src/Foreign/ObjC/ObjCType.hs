{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Foreign.ObjC.ObjCType where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.ObjC.Object
import Foreign.ObjC.SEL
import Foreign.Ptr

class ObjCType t where
    typeString :: p t -> String
    
    ptrTypeString :: p t -> String
    ptrTypeString p = '^' : typeString p

instance ObjCType () where
    typeString _ = "v"

instance ObjCType a => ObjCType (Ptr a) where
    typeString = ptrTypeString . (const Nothing :: p (q a) -> Maybe a)

instance ObjCType (FunPtr a) where
    typeString _ = "?"

instance ObjCType ObjCObject where
    typeString    _ = "{objc_object=#}"
    ptrTypeString _ = "@"

instance ObjCType ObjCClass where
    typeString    _ = "{objc_class=}"
    ptrTypeString _ = "#"

instance ObjCType ObjCSuper where
    typeString    _ = "{objc_super=@#}"

instance ObjCType (SEL a) where
    typeString _ = ":"

instance ObjCType (IMP a) where
    typeString _ = "?"

instance ObjCType Float where
    typeString _ = "f"

instance ObjCType Double where
    typeString _ = "d"

instance ObjCType Int8 where
    typeString _ = "c"

instance ObjCType Int16 where
    typeString _ = "s"

instance ObjCType Int32 where
    typeString _ = "l"

instance ObjCType Int64 where
    typeString _ = "q"

-- TODO: detect int size
instance ObjCType Int where
    typeString _ = "q"

instance ObjCType Word8 where
    typeString _ = "C"

instance ObjCType Word16 where
    typeString _ = "S"

instance ObjCType Word32 where
    typeString _ = "L"

instance ObjCType Word64 where
    typeString _ = "Q"

-- TODO: detect int size
instance ObjCType Word where
    typeString _ = "Q"

instance ObjCType CChar where
    typeString    _ = "c"
    ptrTypeString _ = "*"

deriving instance ObjCType CSChar
deriving instance ObjCType CUChar
deriving instance ObjCType CShort
deriving instance ObjCType CUShort
deriving instance ObjCType CInt
deriving instance ObjCType CUInt
deriving instance ObjCType CLong
deriving instance ObjCType CULong
deriving instance ObjCType CPtrdiff
deriving instance ObjCType CSize
deriving instance ObjCType CWchar
deriving instance ObjCType CSigAtomic
deriving instance ObjCType CLLong
deriving instance ObjCType CULLong
deriving instance ObjCType CIntPtr
deriving instance ObjCType CUIntPtr
deriving instance ObjCType CIntMax
deriving instance ObjCType CUIntMax
deriving instance ObjCType CClock
deriving instance ObjCType CTime
deriving instance ObjCType CUSeconds
deriving instance ObjCType CSUSeconds
deriving instance ObjCType CFloat
deriving instance ObjCType CDouble
