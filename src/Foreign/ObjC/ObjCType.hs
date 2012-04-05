{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Foreign.ObjC.ObjCType where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.LibFFI.Experimental
import Foreign.ObjC.Object
import Foreign.ObjC.SEL
import Foreign.Ptr

class ObjCType t where
    typeString :: p t -> String
    
    ptrTypeString :: p t -> String
    ptrTypeString p = '^' : typeString p

class (ObjCType t, ArgType t) => ObjCArg t where

class (ObjCType t, RetType t) => ObjCRet t where
    isStret :: p t -> Bool
    isStret _ = False

    outRetRetained :: OutRet t t
    outRetRetained = outRet

instance ObjCType () where
    typeString _ = "v"
instance ObjCRet ()

instance ObjCType a => ObjCType (Ptr a) where
    typeString = ptrTypeString . (const Nothing :: p (q a) -> Maybe a)
instance ObjCType a => ObjCArg (Ptr a)
instance ObjCType a => ObjCRet (Ptr a)

instance ObjCType (FunPtr a) where
    typeString _ = "?"
instance ObjCArg (FunPtr a)
instance ObjCRet (FunPtr a)

instance ObjCType ObjCObject where
    typeString    _ = "{objc_object=#}"
    ptrTypeString _ = "@"

instance ObjCType ObjCClass where
    typeString    _ = "{objc_class=}"
    ptrTypeString _ = "#"

instance ObjCType ObjCSuper where
    typeString    _ = "{objc_super=@#}"
instance ObjCArg ObjCSuper
instance ObjCRet ObjCSuper

instance ObjCType (SEL a) where
    typeString _ = ":"
instance ObjCArg (SEL a)
instance ObjCRet (SEL a)

instance ObjCType (IMP a) where
    typeString _ = "?"
instance ObjCArg (IMP a)
instance ObjCRet (IMP a)

instance ObjCType Float where
    typeString _ = "f"
instance ObjCArg Float
instance ObjCRet Float

instance ObjCType Double where
    typeString _ = "d"
instance ObjCArg Double
instance ObjCRet Double

instance ObjCType Int8 where
    typeString _ = "c"
instance ObjCArg Int8
instance ObjCRet Int8

instance ObjCType Int16 where
    typeString _ = "s"
instance ObjCArg Int16
instance ObjCRet Int16

instance ObjCType Int32 where
    typeString _ = "l"
instance ObjCArg Int32
instance ObjCRet Int32

instance ObjCType Int64 where
    typeString _ = "q"
instance ObjCArg Int64
instance ObjCRet Int64

-- TODO: detect int size
instance ObjCType Int where
    typeString _ = "q"
instance ObjCArg Int
instance ObjCRet Int

instance ObjCType Word8 where
    typeString _ = "C"
instance ObjCArg Word8
instance ObjCRet Word8

instance ObjCType Word16 where
    typeString _ = "S"
instance ObjCArg Word16
instance ObjCRet Word16

instance ObjCType Word32 where
    typeString _ = "L"
instance ObjCArg Word32
instance ObjCRet Word32

instance ObjCType Word64 where
    typeString _ = "Q"
instance ObjCArg Word64
instance ObjCRet Word64

-- TODO: detect int size
instance ObjCType Word where
    typeString _ = "Q"
instance ObjCArg Word
instance ObjCRet Word

instance ObjCType CChar where
    typeString    _ = "c"
    ptrTypeString _ = "*"
instance ObjCArg CChar
instance ObjCRet CChar

deriving instance ObjCType CSChar
deriving instance ObjCArg  CSChar
deriving instance ObjCRet  CSChar

deriving instance ObjCType CUChar
deriving instance ObjCArg  CUChar
deriving instance ObjCRet  CUChar

deriving instance ObjCType CShort
deriving instance ObjCArg  CShort
deriving instance ObjCRet  CShort

deriving instance ObjCType CUShort
deriving instance ObjCArg  CUShort
deriving instance ObjCRet  CUShort

deriving instance ObjCType CInt
deriving instance ObjCArg  CInt
deriving instance ObjCRet  CInt

deriving instance ObjCType CUInt
deriving instance ObjCArg  CUInt
deriving instance ObjCRet  CUInt

deriving instance ObjCType CLong
deriving instance ObjCArg  CLong
deriving instance ObjCRet  CLong

deriving instance ObjCType CULong
deriving instance ObjCArg  CULong
deriving instance ObjCRet  CULong

deriving instance ObjCType CPtrdiff
deriving instance ObjCArg  CPtrdiff
deriving instance ObjCRet  CPtrdiff

deriving instance ObjCType CSize
deriving instance ObjCArg  CSize
deriving instance ObjCRet  CSize

deriving instance ObjCType CWchar
deriving instance ObjCArg  CWchar
deriving instance ObjCRet  CWchar

deriving instance ObjCType CSigAtomic
deriving instance ObjCArg  CSigAtomic
deriving instance ObjCRet  CSigAtomic

deriving instance ObjCType CLLong
deriving instance ObjCArg  CLLong
deriving instance ObjCRet  CLLong

deriving instance ObjCType CULLong
deriving instance ObjCArg  CULLong
deriving instance ObjCRet  CULLong

deriving instance ObjCType CIntPtr
deriving instance ObjCArg  CIntPtr
deriving instance ObjCRet  CIntPtr

deriving instance ObjCType CUIntPtr
deriving instance ObjCArg  CUIntPtr
deriving instance ObjCRet  CUIntPtr

deriving instance ObjCType CIntMax
deriving instance ObjCArg  CIntMax
deriving instance ObjCRet  CIntMax

deriving instance ObjCType CUIntMax
deriving instance ObjCArg  CUIntMax
deriving instance ObjCRet  CUIntMax

deriving instance ObjCType CClock
deriving instance ObjCArg  CClock
deriving instance ObjCRet  CClock

deriving instance ObjCType CTime
deriving instance ObjCArg  CTime
deriving instance ObjCRet  CTime

deriving instance ObjCType CUSeconds
deriving instance ObjCArg  CUSeconds
deriving instance ObjCRet  CUSeconds

deriving instance ObjCType CSUSeconds
deriving instance ObjCArg  CSUSeconds
deriving instance ObjCRet  CSUSeconds

deriving instance ObjCType CFloat
deriving instance ObjCArg  CFloat
deriving instance ObjCRet  CFloat

deriving instance ObjCType CDouble
deriving instance ObjCArg  CDouble
deriving instance ObjCRet  CDouble