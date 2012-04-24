{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.ObjC.Raw.Exception where

import Control.Exception
import Foreign.C.String
import Foreign.C.Types
import Foreign.LibFFI.Experimental
import Foreign.ObjC.Types
import Foreign.Ptr
import Foreign.StablePtr

foreign import ccall
    ffi_call_with_exceptions :: CIF a -> FunPtr a -> Ptr (SigReturn a) -> Ptr (Ptr ()) -> Ptr ObjCException -> IO CInt

foreign import ccall
    newHSException :: CString -> StablePtr SomeException -> IO ObjCException
