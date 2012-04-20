{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.ObjC.Raw.Object where

import Foreign.C.String
import Foreign.C.Types
import Foreign.ObjC.Types
import Foreign.Ptr

#ifndef GNUSTEP

foreign import ccall unsafe
    objc_getAssociatedObject :: Id -> Ptr () -> IO Id

foreign import ccall unsafe
    objc_removeAssociatedObjects :: Id -> IO ()

foreign import ccall unsafe
    objc_setAssociatedObject :: Id -> Ptr () -> Id -> ObjCAssociationPolicy -> IO ()

#endif

foreign import ccall unsafe
    object_copy :: Id -> CSize -> IO Id

foreign import ccall unsafe
    object_dispose :: Id -> IO Id

foreign import ccall unsafe
    object_getClass :: Id -> IO Class

foreign import ccall unsafe
    object_getClassName :: Id -> IO CString

foreign import ccall unsafe
    object_getIndexedIvars :: Id -> IO (Ptr ())

foreign import ccall unsafe
    object_getInstanceVariable :: Id -> CString -> Ptr (Ptr a) -> IO Ivar

foreign import ccall unsafe
    object_getIvar :: Id -> Ivar -> IO Id

foreign import ccall unsafe
    object_setClass :: Id -> Class -> IO Class

foreign import ccall unsafe
    object_setInstanceVariable :: Id -> CString -> Ptr a -> IO Ivar

foreign import ccall unsafe
    object_setIvar :: Id -> Ivar -> Id -> IO ()

-- memory managament conveniences
foreign import ccall
    retainObject  :: Ptr ObjCObject -> IO (Ptr ObjCObject)

foreign import ccall
    releaseObject :: Ptr ObjCObject -> IO ()

foreign import ccall
    autoreleaseObject  :: Ptr ObjCObject -> IO (Ptr ObjCObject)
