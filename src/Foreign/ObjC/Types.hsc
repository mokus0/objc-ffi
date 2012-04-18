{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Foreign.ObjC.Types where

import Control.Applicative
import Control.Exception
import Data.Typeable
import Foreign.C.String
import Foreign.C.Types
import Foreign.LibFFI.Experimental
import Foreign.Ptr
import Foreign.Storable

#include <objc/objc.h>

#ifdef GNUSTEP
#include <objc/objc-api.h>
#else
#include <objc/objc-runtime.h>
#endif

-- struct objc_class (abstract as of ObjC 2)
data ObjCClass
type Class = Ptr ObjCClass

-- struct objc_method (abstract as of ObjC 2)
data ObjCMethod a
type Method a = Ptr (ObjCMethod a)

-- struct objc_ivar (abstract as of ObjC 2)
data ObjCIvar
type Ivar = Ptr ObjCIvar

-- struct objc_category (abstract as of ObjC 2)
data ObjCCategory
type Category = Ptr ObjCCategory

#ifndef GNUSTEP

-- (GNUstep doesn't appear to support properties)
-- struct objc_property (abstract as of ObjC 2)
data ObjCProperty
type Property = Ptr ObjCProperty

#endif

newtype IMP a = IMP (FunPtr (Ptr ObjCObject -> SEL a -> a))
    deriving (Eq, Ord, Show, Storable, FFIType, ArgType, RetType)

newtype SEL a = SEL (Ptr ())
    deriving (Eq, Ord, Show, Storable, FFIType, ArgType, RetType)

-- data ObjCMethodList
-- 
-- data ObjCCache
-- 
-- data ObjCProtocolList

-- struct objc_object (semi-abstract as of ObjC 2)
data ObjCObject
type Id = Ptr ObjCObject

-- typedef struct objc_object Protocol
type Protocol = ObjCObject

-- struct objc_method_description
-- TODO: check whether this exists and/or is the same on GNUstep...
data ObjCMethodDescription a = ObjCMethodDescription
    { methodName    :: SEL a
    , methodType    :: CString
    } deriving (Eq, Ord, Show)

instance Storable (ObjCMethodDescription a) where
    sizeOf    _ = #size struct objc_method_description
    alignment   = liftA2 max (alignment . methodName) (alignment . methodType) 
    peek p = do
        methodName <- (#peek struct objc_method_description, name)  p
        methodType <- (#peek struct objc_method_description, types) p
        return ObjCMethodDescription{..}
    poke p ObjCMethodDescription{..} = do
        (#poke struct objc_method_description, name)  p methodName
        (#poke struct objc_method_description, types) p methodType

instance FFIType (ObjCMethodDescription a) where
    ffiType = Type (struct [ffiTypeOf_ methodName, ffiTypeOf_ methodType])
instance ArgType (ObjCMethodDescription a)
instance RetType (ObjCMethodDescription a)

-- struct objc_super
data ObjCSuper = ObjCSuper
    { receiver  :: !(Ptr ObjCObject)
    , super     :: !(Ptr ObjCClass)
    }

instance Storable ObjCSuper where
    sizeOf      _ = #size  struct objc_super
    alignment     = liftA2 max (alignment . receiver) (alignment . super)

#ifdef GNUSTEP
    peek p = do
        receiver    <- (#peek struct objc_super, self)  p
        super       <- (#peek struct objc_super, class) p
        return ObjCSuper{..}
    poke p ObjCSuper{..} = do
        (#poke struct objc_super, self)  p receiver
        (#poke struct objc_super, class) p super
#else
    peek p = do
        receiver    <- (#peek struct objc_super, receiver)    p
        super       <- (#peek struct objc_super, super_class) p
        return ObjCSuper{..}
    poke p ObjCSuper{..} = do
        (#poke struct objc_super, receiver)    p receiver
        (#poke struct objc_super, super_class) p super
#endif

instance FFIType ObjCSuper where
    ffiType = Type (struct [ffiTypeOf_ receiver, ffiTypeOf_ super])
instance ArgType ObjCSuper
instance RetType ObjCSuper

newtype ObjCAssociationPolicy = ObjCAssociationPolicy CUIntPtr

toObjCBool :: Bool -> CSChar
toObjCBool False = 0
toObjCBool True  = 1

fromObjCBool :: CSChar -> Bool
fromObjCBool 0 = False
fromObjCBool 1 = True

newtype ObjCException = ObjCException (Ptr ObjCObject)
    deriving (Eq, Show, Typeable, Storable)
instance Exception ObjCException

