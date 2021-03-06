{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.ObjC.Raw.Property where

import Foreign.C.String
import Foreign.C.Types
import Foreign.ObjC.Types
import Foreign.Ptr

foreign import ccall unsafe
    class_copyPropertyList :: Class -> Ptr CUInt -> IO (Ptr Property)

foreign import ccall unsafe
    class_getProperty :: Class -> CString -> IO Property

foreign import ccall unsafe
    property_getAttributes :: Property -> IO CString

foreign import ccall unsafe
    property_getName :: Property -> IO CString

foreign import ccall unsafe
    protocol_copyPropertyList :: Ptr Protocol -> Ptr CUInt -> IO (Ptr Property)

foreign import ccall unsafe
    protocol_getProperty :: Ptr Protocol -> CString -> CSChar -> CSChar -> IO Property
