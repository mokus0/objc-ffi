{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.ObjC.Class where

import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ObjC.Types
import Foreign.Ptr
import GHC.Base (Any)

foreign import ccall unsafe
    class_addIvar :: Class -> CString -> CSize -> Word8 -> CString -> IO CSChar

foreign import ccall unsafe
    class_addMethod :: Class -> SEL a -> IMP a -> CString -> IO CSChar

foreign import ccall unsafe
    class_addProtocol :: Class -> Ptr Protocol -> IO CSChar

foreign import ccall unsafe
    class_conformsToProtocol :: Class -> Ptr Protocol -> IO CSChar

foreign import ccall unsafe
    class_copyIvarList :: Class -> Ptr CUInt -> IO Ivar

foreign import ccall unsafe
    class_copyMethodList :: Class -> Ptr CUInt -> IO (Ptr (ObjCMethod Any))

foreign import ccall unsafe
    class_copyProtocolList :: Class -> Ptr CUInt -> IO (Ptr (Ptr Protocol))

foreign import ccall unsafe
    class_createInstance :: Class -> CSize -> IO Id

foreign import ccall unsafe
    class_getClassMethod :: Class -> SEL a -> IO (Method a)

foreign import ccall unsafe
    class_getClassVariable :: Class -> CString -> IO Ivar

foreign import ccall unsafe
    class_getInstanceMethod :: Class -> SEL a -> IO (Method a)

foreign import ccall unsafe
    class_getInstanceSize :: Class -> IO CSize

foreign import ccall unsafe
    class_getInstanceVariable :: Class -> CString -> IO Ivar

foreign import ccall unsafe
    class_getIvarLayout :: Class -> IO (Ptr CChar) -- is this a CString?

foreign import ccall unsafe
    class_getMethodImplementation :: Class -> SEL a -> IO (IMP a)

foreign import ccall unsafe
    class_getMethodImplementation_stret :: Class -> SEL a -> IO (IMP a)

foreign import ccall unsafe
    class_getName :: Class -> IO CString

foreign import ccall unsafe
    class_getSuperclass :: Class -> IO Class

foreign import ccall unsafe
    class_getVersion :: Class -> IO CInt

foreign import ccall unsafe
    class_getWeakIvarLayout :: Class -> IO (Ptr CChar) -- CString?

foreign import ccall unsafe
    class_isMetaClass :: Class -> IO CSChar

foreign import ccall unsafe
    class_replaceMethod :: Class -> SEL a -> IMP a -> CString -> IO (IMP a)

foreign import ccall unsafe
    class_respondsToSelector :: Class -> SEL a -> IO CSChar

foreign import ccall unsafe
    class_setIvarLayout :: Class -> CString -> IO ()

foreign import ccall unsafe
    class_setSuperclass :: Class -> Class -> IO Class

foreign import ccall unsafe
    class_setVersion :: Class -> CInt -> IO ()

foreign import ccall unsafe
    class_setWeakIvarLayout :: Class -> CString -> IO ()

foreign import ccall unsafe
    objc_allocateClassPair :: Class -> CString -> CSize -> IO Class

foreign import ccall unsafe
    objc_getClass :: CString -> IO Class

foreign import ccall unsafe
    objc_getClassList :: Ptr Class -> CInt -> IO CInt

foreign import ccall unsafe
    objc_getMetaClass :: CString -> IO Id

foreign import ccall unsafe
    objc_getRequiredClass :: CString -> IO Id

foreign import ccall unsafe
    objc_lookUpClass :: CString -> IO Id

foreign import ccall unsafe
    objc_registerClassPair :: Class -> IO ()
