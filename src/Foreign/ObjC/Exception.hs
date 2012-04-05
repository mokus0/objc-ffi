{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.ObjC.Exception where

import Control.Exception
import Data.Typeable
import Foreign.C.Types
import Foreign.LibFFI.Experimental
import Foreign.Marshal.Alloc
import Foreign.ObjC.Object
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable

newtype ObjCException = ObjCException (Ptr ObjCObject)
    deriving (Eq, Show, Typeable, Storable)
instance Exception ObjCException

foreign import ccall ffi_call_with_exceptions :: CIF a -> FunPtr a -> Ptr (SigReturn a) -> Ptr (Ptr ()) -> Ptr ObjCException -> IO CInt

foreign export ccall freeStablePtr :: StablePtr a -> IO ()

objc_ffi_call :: CIF a -> FunPtr a -> Ptr (SigReturn a) -> Ptr (Ptr ()) -> IO ()
objc_ffi_call cif impl ret args = alloca $ \exc -> do
    mbExc <- ffi_call_with_exceptions cif impl ret args exc
    
    case mbExc of
        0 -> return ()
        1 -> do
            ObjCException p <- peek exc
            let sptr :: StablePtr SomeException
                sptr = castPtrToStablePtr (castPtr p)
            deRefStablePtr sptr >>= throwIO
        2 -> do
            peek exc >>= throwIO

callWithExceptions :: Dynamic a => FunPtr a -> a
callWithExceptions = importDynWithCall (objc_ffi_call cif) dyn