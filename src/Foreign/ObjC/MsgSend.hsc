{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.ObjC.MsgSend where

import Foreign.LibFFI.Experimental
import Foreign.Marshal
import Foreign.ObjC.Exception
import Foreign.ObjC.Types
import Foreign.Ptr

#include <ffi.h>

msgSend      :: Dynamic a => Ptr ObjCObject -> SEL a -> a
msgSendSuper :: Dynamic a => ObjCSuper -> SEL a -> a

#ifdef GNUSTEP

foreign import ccall unsafe objc_msg_lookup       :: Ptr ObjCObject -> SEL a -> IO (IMP a)
foreign import ccall unsafe objc_msg_lookup_super :: Ptr ObjCSuper  -> SEL a -> IO (IMP a)

msgSend obj sel = callWithExceptions imp obj sel
    where
        IMP imp  = unsafePerformIO (objc_msg_lookup obj sel)

msgSendSuper super sel = callWithExceptions imp (receiver super) sel
    where
        IMP imp  = unsafePerformIO $
            with super $ \super ->
                objc_msg_lookup_super super sel

#else

foreign import ccall "&" objc_msgSend               :: FunPtr (Ptr ObjCObject -> SEL a -> a)
foreign import ccall "&" objc_msgSendSuper          :: FunPtr (Ptr ObjCSuper  -> SEL a -> a)

-- arm:    unknown
-- i386:   any struct return type larger than 8 bytes
-- x86-64: any struct return type where classify_argument == MEMORY
--          (with possible exception on certain compilers that seem to 
--           incorrectly classify structs with 16-32 bytes as "MEMORY")
foreign import ccall "&" objc_msgSend_stret         :: FunPtr (Ptr ObjCObject -> SEL a -> a)
foreign import ccall "&" objc_msgSendSuper_stret    :: FunPtr (Ptr ObjCSuper  -> SEL a -> a)

-- arm:    not used
-- i386:   used for `float`, `double`, `long double`.
-- x86-64: used for `long double`.
-- (never used for messages to 'super')
foreign import ccall "&" objc_msgSend_fpret         :: FunPtr (Ptr ObjCObject -> SEL a -> a)

-- arm:    not used
-- i386:   not used
-- x86-64: used for `_Complex long double`.
-- (never used for messages to 'super')
foreign import ccall "&" objc_msgSend_fp2ret        :: FunPtr (Ptr ObjCObject -> SEL a -> a)

cifIsStret  :: CIF a -> Bool
cifIsFpret  :: CIF a -> Bool
cifIsFp2ret :: CIF a -> Bool

#ifdef __x86_64__

-- TODO: check these on OS X
-- There seems to be some confusion about the ABI - GCC changed 
-- its implementation at some point, and I'm not entirely sure we
-- can be sure that the libffi we're using is in sync with the
-- gcc we're using...

cifIsStret (CIF cif) 
    =  typeIsStruct (retType cif)
    && cifFlags cif == #const FFI_TYPE_VOID

cifIsFpret  (CIF cif) = typeType (retType cif) == #const FFI_TYPE_LONGDOUBLE
cifIsFp2ret (CIF cif) = False 
    -- actually, this should be true if the return type is "complex long double",
    -- but I'm not sure how to tell if that is the case from the info in a ffi_cif

#elif defined(__i386__)

cifIsStret  (CIF cif) = typeSize (retType cif) > 8
cifIsFpret  (CIF cif) = case typeType (retType cif) of
    (#const FFI_TYPE_FLOAT)         -> True
    (#const FFI_TYPE_DOUBLE)        -> True
    (#const FFI_TYPE_LONGDOUBLE)    -> True
    _                               -> False
cifIsFp2ret _ = False

#else
#error Unrecognized or unsupported architecture
#endif

-- TODO: test on Mac OS
-- TODO: this stuff can probably be made faster by just making
--       msgSend and msgSendSuper (or the corresponding FunPtr) 
--       methods of ObjCRet and statically determining the correct
--       one to use.

msgSend obj sel = importDynWithCall (objc_ffi_call theCIF) dyn send obj sel
    where
        {-# NOINLINE theCIF #-}
        theCIF = cif
        
        send = if cifIsStret theCIF
            then objc_msgSend_stret
            else if cifIsFpret theCIF
                then objc_msgSend_fpret
                else if cifIsFp2ret theCIF
                    then objc_msgSend_fp2ret
                    else objc_msgSend

msgSendSuper super sel = importDynWithCall (objc_ffi_call  theCIF) (superArg `consDyn` dyn) send super sel
    where
        superArg :: OutArg (Ptr ObjCSuper) ObjCSuper
        superArg = outByRef (OutArg with)
        
        {-# NOINLINE theCIF #-}
        theCIF = cif
        
        {-# NOINLINE send#-}
        send = if cifIsStret theCIF
            then objc_msgSendSuper_stret
            else objc_msgSendSuper

#endif
