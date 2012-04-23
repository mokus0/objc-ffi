module Foreign.ObjC.Exception where

import Control.Exception
import Foreign.C.Types
import Foreign.LibFFI.Experimental
import Foreign.Marshal.Alloc
import qualified Foreign.ObjC.Raw.Exception as Raw
import Foreign.ObjC.Types
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable

unwrapException :: (Ptr ObjCException -> IO CInt) -> IO ()
unwrapException action = alloca $ \exc -> do
    mbExc <- action exc
    
    case mbExc of
        0 -> return ()
        1 -> do
            ObjCException p <- peek exc
            let sptr :: StablePtr SomeException
                sptr = castPtrToStablePtr (castPtr p)
            deRefStablePtr sptr >>= throwIO
        2 -> do
            peek exc >>= throwIO

objc_ffi_call :: CIF a -> FunPtr a -> Ptr (SigReturn a) -> Ptr (Ptr ()) -> IO ()
objc_ffi_call cif impl ret args =
    unwrapException (Raw.ffi_call_with_exceptions cif impl ret args)

callWithExceptions :: Dynamic a => FunPtr a -> a
callWithExceptions = importDynWithCall (objc_ffi_call cif) dyn