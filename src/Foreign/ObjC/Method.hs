{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.ObjC.Method where

import Foreign.C.String
import Foreign.C.Types
import Foreign.ObjC.Types
import Foreign.Ptr

foreign import ccall unsafe
    method_copyArgumentType :: Method a -> CUInt -> IO CString -- must be freed

foreign import ccall unsafe
    method_copyReturnType :: Method a -> IO CString -- must be freed

foreign import ccall unsafe
    method_exchangeImplementations :: Method a -> Method a -> IO ()

foreign import ccall unsafe
    method_getArgumentType :: Method a -> CUInt -> Ptr CChar -> CSize -> IO ()

foreign import ccall unsafe
    method_getImplementation :: Method a -> IO (IMP a)

foreign import ccall unsafe
    method_getName :: Method a -> IO (SEL a)

foreign import ccall unsafe
    method_getNumberOfArguments :: Method a -> IO CUInt

foreign import ccall unsafe
    method_getReturnType :: Method a -> Ptr CChar -> CSize -> IO ()

foreign import ccall unsafe
    method_getTypeEncoding :: Method a -> IO CString

foreign import ccall unsafe
    method_setImplementation :: Method a -> IMP a -> IO (IMP a)
