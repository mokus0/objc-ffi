module Foreign.ObjC.Property where

import Control.Monad
import Foreign.C.String
import Foreign.ObjC.Raw.Utils
import Foreign.ObjC.Types
import Foreign.Ptr
import qualified Foreign.ObjC.Raw.Property as Raw

class_getPropertyList :: Class -> IO [Property]
class_getPropertyList = wrapCopyListFn . Raw.class_copyPropertyList

class_getProperty :: Class -> String -> IO Property
class_getProperty cls name =
    withCString name $ \name ->
        Raw.class_getProperty cls name

property_getAttributes :: Property -> IO String
property_getAttributes = peekCString <=< Raw.property_getAttributes

property_getName :: Property -> IO String
property_getName = peekCString <=< Raw.property_getName

protocol_copyPropertyList :: Ptr Protocol -> IO [Property]
protocol_copyPropertyList = wrapCopyListFn . Raw.protocol_copyPropertyList

protocol_getProperty :: Ptr Protocol -> String -> Bool -> Bool -> IO Property
protocol_getProperty proto name isReqProp isInstProp =
    withCString name $ \name ->
        Raw.protocol_getProperty proto name (toObjCBool isReqProp) (toObjCBool isInstProp)
