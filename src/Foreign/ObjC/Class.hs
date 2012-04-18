{-# LANGUAGE CPP #-}
module Foreign.ObjC.Class where

import Control.Monad
import Data.List
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal
import Foreign.ObjC.ObjCType
import Foreign.ObjC.SEL
import Foreign.ObjC.Types
import Foreign.Ptr
import Foreign.Storable
import GHC.Base (Any)
import qualified Foreign.ObjC.Raw.Class as Raw

class_addIvar :: (ObjCType t, Storable t) => Class -> String -> proxy t -> IO Bool
class_addIvar cls name ivarType =
    fmap fromObjCBool $
        withCString name $ \name ->
            withCString (typeString ivarType) $ \typeStr -> 
                Raw.class_addIvar cls name (fromIntegral sz) (lg2 align) typeStr
    where
        t = (undefined :: p t -> t) ivarType
        
        sz    = sizeOf    t
        align = alignment t
        
        lg2 :: Int -> Word8
#ifdef GNUSTEP
            -- wonky hack for a bug in GNUstep... 
            -- doesn't really fix the problem, just makes it slightly less frequent
            -- (by forcing all ivars under 64 bytes to be treated as unaligned, which
            -- makes objects much larger than they otherwise would be)
            -- the bug is fixed in GNUstep's SVN repo... eventually we can remove this
            -- hack.
        lg2 = const 6
#else
        lg2 x = maybe 0 fromIntegral (findIndex (\i -> i >= x || i <= 0) (iterate (2 *) 1))
#endif

class_addMethod :: ObjCSigType a => Class -> SEL a -> IMP a -> IO Bool
class_addMethod cls sel imp = 
    fmap fromObjCBool $
        withCString (selTypeString sel) $ \typeStr ->
            Raw.class_addMethod cls sel imp typeStr

class_addProtocol :: Class -> Ptr Protocol -> IO Bool
class_addProtocol cls protocol =
    fmap fromObjCBool $
        Raw.class_addProtocol cls protocol

class_conformsToProtocol :: Class -> Ptr Protocol -> IO Bool
class_conformsToProtocol cls protocol =
    fmap fromObjCBool $
        Raw.class_conformsToProtocol cls protocol

wrapCopyListFn :: (Integral a, Storable a, Storable b) => (Ptr a -> IO (Ptr b)) -> IO [b]
wrapCopyListFn copyList =
    alloca $ \outCount -> do
        buf <- copyList outCount
        
        n      <- peek outCount
        things <- peekArray (fromIntegral n) buf
        free buf
        return things

class_getIvarList :: Class -> IO [Ivar]
class_getIvarList = wrapCopyListFn . Raw.class_copyIvarList

class_getMethodList :: Class -> IO [Method Any]
class_getMethodList = wrapCopyListFn . Raw.class_copyMethodList

class_getProtocolList :: Class -> IO [Ptr Protocol]
class_getProtocolList = wrapCopyListFn . Raw.class_copyProtocolList

class_createInstance :: Class -> CSize -> IO Id
class_createInstance = Raw.class_createInstance

class_getClassMethod :: Class -> SEL a -> IO (Method a)
class_getClassMethod = Raw.class_getClassMethod

class_getClassVariable :: Class -> String -> IO Ivar
class_getClassVariable cls name =
    withCString name $ \name ->
        Raw.class_getClassVariable cls name

class_getInstanceMethod :: Class -> SEL a -> IO (Method a)
class_getInstanceMethod = Raw.class_getInstanceMethod

class_getInstanceSize :: Class -> IO CSize
class_getInstanceSize = Raw.class_getInstanceSize

class_getInstanceVariable :: Class -> String -> IO Ivar
class_getInstanceVariable cls name =
    withCString name $ \name ->
        Raw.class_getInstanceVariable cls name

class_getIvarLayout :: Class -> IO String
class_getIvarLayout = peekCString <=< Raw.class_getIvarLayout

class_getMethodImplementation :: Class -> SEL a -> IO (IMP a)
class_getMethodImplementation = Raw.class_getMethodImplementation

class_getMethodImplementation_stret :: Class -> SEL a -> IO (IMP a)
class_getMethodImplementation_stret = Raw.class_getMethodImplementation_stret

class_getName :: Class -> IO String
class_getName = peekCString <=< Raw.class_getName

class_getSuperclass :: Class -> IO Class
class_getSuperclass = Raw.class_getSuperclass

class_getVersion :: Class -> IO CInt
class_getVersion = Raw.class_getVersion

class_getWeakIvarLayout :: Class -> IO String
class_getWeakIvarLayout = peekCString <=< Raw.class_getWeakIvarLayout

class_isMetaClass :: Class -> IO Bool
class_isMetaClass = fmap fromObjCBool . Raw.class_isMetaClass

class_replaceMethod :: ObjCSigType a => Class -> SEL a -> IMP a -> IO (IMP a)
class_replaceMethod cls sel imp = 
    withCString (selTypeString sel) $ \typeStr ->
        Raw.class_replaceMethod cls sel imp typeStr

class_respondsToSelector :: Class -> SEL a -> IO Bool
class_respondsToSelector cls sel =
    fmap fromObjCBool $
        Raw.class_respondsToSelector cls sel

class_setIvarLayout :: Class -> String -> IO ()
class_setIvarLayout cls layout =
    withCString layout $ \layout ->
        Raw.class_setIvarLayout cls layout

class_setSuperclass :: Class -> Class -> IO Class
class_setSuperclass = Raw.class_setSuperclass

class_setVersion :: Class -> CInt -> IO ()
class_setVersion = Raw.class_setVersion

class_setWeakIvarLayout :: Class -> String -> IO ()
class_setWeakIvarLayout cls layout =
    withCString layout $ \layout ->
        Raw.class_setWeakIvarLayout cls layout

objc_allocateClassPair :: Class -> String -> CSize -> IO Class
objc_allocateClassPair super name extraBytes = 
    withCString name $ \name ->
        Raw.objc_allocateClassPair super name extraBytes

objc_getClass :: String -> IO Class
objc_getClass name = withCString name Raw.objc_getClass

wrapGetListFn :: (Integral a, Storable a, Storable b) => (Ptr b -> a -> IO a) -> IO [b]
wrapGetListFn getList = do
    n <- getList nullPtr 0
    
    buf <- mallocArray (fromIntegral n)
    getList buf n
    
    things <- peekArray (fromIntegral n) buf
    free buf
    return things

objc_getClassList :: IO [Class]
objc_getClassList = wrapGetListFn Raw.objc_getClassList

objc_getMetaClass :: String -> IO Id
objc_getMetaClass name = withCString name Raw.objc_getMetaClass

objc_getRequiredClass :: String -> IO Id
objc_getRequiredClass name = withCString name Raw.objc_getRequiredClass

objc_lookUpClass :: String -> IO Id
objc_lookUpClass name = withCString name Raw.objc_lookUpClass

objc_registerClassPair :: Class -> IO ()
objc_registerClassPair = Raw.objc_registerClassPair

