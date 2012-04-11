{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.ObjC.Property where

import Foreign.C.String
import Foreign.ObjC.Types

foreign import ccall unsafe
    property_getAttributes :: Property -> IO CString

foreign import ccall unsafe
    property_getName :: Property -> IO CString
