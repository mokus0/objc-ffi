{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foreign.ObjC.SEL where

import Foreign.LibFFI.Experimental
import Foreign.Ptr
import Foreign.Storable

newtype SEL = SEL (Ptr ())
    deriving (Eq, Ord, Show, Storable, FFIType, ArgType, RetType)

