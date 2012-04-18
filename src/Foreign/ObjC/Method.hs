module Foreign.ObjC.Method where

import Control.Monad
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.ObjC.Types
import qualified Foreign.ObjC.Raw.Method as Raw

method_getArgumentType :: Method a -> CUInt -> IO String
method_getArgumentType method i = do
    cstr <- Raw.method_copyArgumentType method i
    str <- peekCString cstr
    free cstr
    return str

method_getReturnType :: Method a -> IO String
method_getReturnType method = do
    cstr <- Raw.method_copyReturnType method
    str <- peekCString cstr
    free cstr
    return str

method_exchangeImplementations :: Method a -> Method a -> IO ()
method_exchangeImplementations = Raw.method_exchangeImplementations

method_getImplementation :: Method a -> IO (IMP a)
method_getImplementation = Raw.method_getImplementation

method_getName :: Method a -> IO (SEL a)
method_getName = Raw.method_getName

method_getNumberOfArguments :: Method a -> IO CUInt
method_getNumberOfArguments = Raw.method_getNumberOfArguments

method_getTypeEncoding :: Method a -> IO String
method_getTypeEncoding = peekCString <=< Raw.method_getTypeEncoding

method_setImplementation :: Method a -> IMP a -> IO (IMP a)
method_setImplementation = Raw.method_setImplementation
