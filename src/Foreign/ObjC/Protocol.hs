module Foreign.ObjC.Protocol where

import Control.Monad
import Foreign.C.String
import Foreign.Dynamic
import qualified Foreign.ObjC.Raw.Protocol as Raw
import Foreign.ObjC.Raw.Utils
import Foreign.ObjC.Types
import Foreign.Ptr
import GHC.Base (Any)

objc_getProtocolList :: IO [Ptr Protocol]
objc_getProtocolList = wrapCopyListFn Raw.objc_copyProtocolList

objc_getProtocol :: String -> IO (Ptr Protocol)
objc_getProtocol name = withCString name Raw.objc_getProtocol

protocol_conformsToProtocol :: Ptr Protocol -> Ptr Protocol -> IO Bool
protocol_conformsToProtocol p1 p2 =
    fmap fromObjCBool $
        Raw.protocol_conformsToProtocol p1 p2

protocol_getMethodDescriptionList :: Ptr Protocol -> Bool -> Bool -> IO [(SEL Any, String)]
protocol_getMethodDescriptionList proto req inst = do
    ms <- wrapCopyListFn (Raw.protocol_copyMethodDescriptionList proto (toObjCBool req) (toObjCBool inst))
    sequence
        [ do
            typeStr <- peekCString typeStr
            return (name, typeStr)
        | ObjCMethodDescription name typeStr <- ms
        ]

protocol_getProtocolList :: Ptr Protocol -> IO [Ptr Protocol]
protocol_getProtocolList = wrapCopyListFn . Raw.protocol_copyProtocolList

protocol_getMethodDescription :: Ptr Protocol -> SEL a -> Bool -> Bool -> IO (SEL Any, String)
protocol_getMethodDescription proto sel req inst = do
    ObjCMethodDescription name typeStr <- dynamic 
        Raw.protocol_getMethodDescription proto sel (toObjCBool req) (toObjCBool inst)
    typeStr <- peekCString typeStr
    return (name, typeStr)

protocol_getName :: Ptr Protocol -> IO String
protocol_getName = peekCString <=< Raw.protocol_getName

protocol_isEqual :: Ptr Protocol -> Ptr Protocol -> IO Bool
protocol_isEqual p1 p2 =
    fmap fromObjCBool $
        Raw.protocol_isEqual p1 p2
