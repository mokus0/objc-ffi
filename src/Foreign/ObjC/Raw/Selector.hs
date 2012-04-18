{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.ObjC.Raw.Selector where

import Foreign.C.String
import Foreign.C.Types
import Foreign.ObjC.Types

foreign import ccall unsafe
    sel_getName :: SEL a -> IO CString

foreign import ccall unsafe
    sel_isEqual :: SEL a -> SEL b -> IO CSChar

foreign import ccall unsafe
    sel_registerName :: CString -> IO (SEL a)
