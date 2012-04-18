{-# LANGUAGE CPP #-}
module Foreign.ObjC.Object where

import qualified Foreign.ObjC.Raw.Object as Raw

import Control.Monad
import Foreign.C.String
import Foreign.C.Types
import Foreign.ObjC.Types
import Foreign.Ptr

#ifndef GNUSTEP

objc_getAssociatedObject :: Id -> Ptr () -> IO Id
objc_getAssociatedObject = Raw.objc_getAssociatedObject

objc_removeAssociatedObjects :: Id -> IO ()
objc_removeAssociatedObjects = Raw.objc_removeAssociatedObjects

objc_setAssociatedObject :: Id -> Ptr () -> Id -> ObjCAssociationPolicy -> IO ()
objc_setAssociatedObject = Raw.objc_setAssociatedObject

#endif

object_copy :: Id -> CSize -> IO Id
object_copy = Raw.object_copy

object_dispose :: Id -> IO Id
object_dispose = Raw.object_dispose

object_getClass :: Id -> IO Class
object_getClass = Raw.object_getClass

object_getClassName :: Id -> IO String
object_getClassName = peekCString <=< Raw.object_getClassName

object_getIndexedIvars :: Id -> IO (Ptr ())
object_getIndexedIvars = Raw.object_getIndexedIvars

object_getInstanceVariable :: Id -> String -> Ptr (Ptr a) -> IO Ivar
object_getInstanceVariable self name p = 
    withCString name $ \name ->
        Raw.object_getInstanceVariable self name p

object_getIvar :: Id -> Ivar -> IO Id
object_getIvar = Raw.object_getIvar

object_setClass :: Id -> Class -> IO Class
object_setClass = Raw.object_setClass

object_setInstanceVariable :: Id -> String -> Ptr a -> IO Ivar
object_setInstanceVariable self name p = 
    withCString name $ \name ->
        Raw.object_setInstanceVariable self name p

object_setIvar :: Id -> Ivar -> Id -> IO ()
object_setIvar = Raw.object_setIvar
