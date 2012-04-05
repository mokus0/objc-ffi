{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RecordWildCards #-}
module Foreign.ObjC.Object where

import Control.Applicative
import Foreign.LibFFI.Experimental
import Foreign.Ptr
import Foreign.Storable

#include <objc/objc.h>

#ifdef GNUSTEP
#include <objc/objc-api.h>
#else
#include <objc/objc-runtime.h>
#include <objc/message.h>
#endif

-- struct objc_class (abstract as of ObjC 2)
data ObjCClass

-- struct objc_object (semi-abstract as of ObjC 2)
data ObjCObject

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
