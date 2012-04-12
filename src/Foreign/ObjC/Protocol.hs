{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.ObjC.Protocol where

import Foreign.C.String
import Foreign.C.Types
import Foreign.ObjC.Types
import Foreign.Ptr

foreign import ccall unsafe
    objc_copyProtocolList :: Ptr CUInt -> IO (Ptr (Ptr Protocol)) -- must be freed

foreign import ccall unsafe
    objc_getProtocol :: CString -> IO (Ptr Protocol)

foreign import ccall unsafe
    protocol_conformsToProtocol :: Ptr Protocol -> Ptr Protocol -> IO CSChar

foreign import ccall unsafe
    protocol_copyMethodDescriptionList :: Ptr Protocol -> CSChar -> CSChar -> Ptr CUInt -> IO (Ptr ObjCMethodDescription)

#ifndef GNUSTEP

foreign import ccall unsafe
    protocol_copyPropertyList :: Ptr Protocol -> Ptr CUInt -> Ptr Property

#endif

foreign import ccall unsafe
    protocol_copyProtocolList :: Ptr Protocol -> Ptr CUInt -> IO (Ptr (Ptr Protocol))

foreign import ccall "&"
    protocol_getMethodDescription :: FunPtr (Ptr Protocol -> SEL a -> CSChar -> CSChar -> IO ObjCMethodDescription)

foreign import ccall unsafe
    protocol_getName :: Ptr Protocol -> IO CString

#ifndef GNUSTEP

foreign import ccall unsafe
    protocol_getProperty :: Ptr Protocol -> CString -> CSChar -> CSChar -> IO Property

#endif

foreign import ccall unsafe
    protocol_isEqual :: Ptr Protocol -> Ptr Protocol -> IO CSChar
