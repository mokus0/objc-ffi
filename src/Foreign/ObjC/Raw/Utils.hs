module Foreign.ObjC.Raw.Utils where

import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

wrapCopyListFn :: (Integral a, Storable a, Storable b) => (Ptr a -> IO (Ptr b)) -> IO [b]
wrapCopyListFn copyList =
    alloca $ \outCount -> do
        buf <- copyList outCount
        
        n      <- peek outCount
        things <- peekArray (fromIntegral n) buf
        free buf
        return things

wrapGetListFn :: (Integral a, Storable a, Storable b) => (Ptr b -> a -> IO a) -> IO [b]
wrapGetListFn getList = do
    n <- getList nullPtr 0
    
    buf <- mallocArray (fromIntegral n)
    getList buf n
    
    things <- peekArray (fromIntegral n) buf
    free buf
    return things

