module Foreign.ObjC.Selector
    ( SEL(..)
    , castSEL
    , getSEL
    , selName
    , ObjCSigType
    , selTypeString
    , IMP(..)
    ) where

import Foreign.C.String
import qualified Foreign.ObjC.Raw.Selector as Raw
import Foreign.ObjC.Sig
import Foreign.ObjC.Types
import Foreign.Ptr
import System.IO.Unsafe

castSEL :: SEL a -> SEL b
castSEL (SEL p) = SEL p

getSEL :: String -> SEL a
getSEL name = unsafePerformIO (withCString name Raw.sel_registerName)

selName :: SEL a -> String
selName sel = unsafePerformIO (peekCString =<< Raw.sel_getName sel)

selTypeString :: ObjCSigType t => SEL t -> String
selTypeString = sigTypeString . (undefined :: SEL t -> p (Ptr ObjCObject -> SEL t -> t))
