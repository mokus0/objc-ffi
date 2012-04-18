module Foreign.ObjC.Ivar where

import Control.Monad
import Foreign.C.String
import Foreign.C.Types
import qualified Foreign.ObjC.Raw.Ivar as Raw
import Foreign.ObjC.Types

ivar_getName :: Ivar -> IO String
ivar_getName = peekCString <=< Raw.ivar_getName

ivar_getOffset :: Ivar -> IO CPtrdiff
ivar_getOffset = Raw.ivar_getOffset

ivar_getTypeEncoding :: Ivar -> IO String
ivar_getTypeEncoding = peekCString <=< Raw.ivar_getTypeEncoding
