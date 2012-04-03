module Foreign.ObjC.ObjCType where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.LibFFI.Experimental
import Foreign.ObjC.SEL
import Foreign.Ptr

class FFIType t => ObjCType t where
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

instance ObjCType SEL where
    typeString _ = ":"
instance ObjCArg SEL
instance ObjCRet SEL

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

