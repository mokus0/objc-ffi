{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.ObjC.SEL
    ( SEL(..)
    , castSEL
    , getSEL
    , selName
    , selTypeString
    , sel_isEqual
    , IMP(..)
    ) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.ObjC.Types
import Foreign.ObjC.Sig
import Foreign.Ptr
import System.IO.Unsafe

castSEL :: SEL a -> SEL b
castSEL (SEL p) = SEL p

foreign import ccall unsafe sel_registerName :: CString -> IO (SEL a)

getSEL :: String -> SEL a
getSEL name = unsafePerformIO (withCString name sel_registerName)

foreign import ccall unsafe
    sel_getName :: SEL a -> IO CString

selName :: SEL a -> String
selName sel = unsafePerformIO $ do
    name <- sel_getName sel
    peekCString name

foreign import ccall unsafe
    sel_isEqual :: SEL a -> SEL b -> IO CSChar

selTypeString :: ObjCSigType t => SEL t -> String
selTypeString = sigTypeString . (undefined :: SEL t -> p (Ptr ObjCObject -> SEL t -> t))
