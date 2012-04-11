{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.ObjC.Ivar where

import Foreign.C.String
import Foreign.C.Types
import Foreign.ObjC.Types

foreign import ccall unsafe
    ivar_getName :: Ivar -> IO CString

foreign import ccall unsafe
    ivar_getOffset :: Ivar -> IO CPtrdiff

foreign import ccall unsafe
    ivar_getTypeEncoding :: Ivar -> IO CString
