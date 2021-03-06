{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.ObjC.HSObject
    ( HSO, nilHSO, withHSO, hsoData, addHSOFinalizer
    , registerHSObjectClass
    , implementMemoryManagement
    , importObject
    , importObject_
    ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Dynamic
import Data.Maybe
import Foreign.C.Types
import Foreign.Concurrent
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Marshal.Alloc
import Foreign.ObjC
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import System.IO
import System.IO.Unsafe
import System.Mem.Weak

data HSO
    = HSO {-# UNPACK #-} !(ForeignPtr ObjCObject) ![Dynamic]
    | Nil

nilHSO :: HSO
nilHSO = Nil

instance Eq HSO where
    HSO a _ == HSO b _  = (a == b)
    Nil     == Nil      = True
    _       == _        = False

instance Show HSO where
    showsPrec _ (HSO fp _) = 
        showChar '<'
        . showString cls
        . showChar ' '
        . shows fp
        . showChar '>'
        where cls = unsafePerformIO (withForeignPtr fp object_getClassName)
    showsPrec _ Nil = showString "nil"

-- TODO: this is probably overkill.  Trying to make sure the HSO stays
-- alive throughout the whole call, in case something in the call is going
-- to retain it.
withHSO :: HSO -> (Ptr ObjCObject -> IO a) -> IO a
withHSO Nil action = action nullPtr
withHSO hso action = 
    bracket (newStablePtr hso) freeStablePtr
    (const (withForeignPtr (fp hso) action))
    where
        {-# NOINLINE fp #-}
        fp (HSO p _) = p

{-# NOINLINE hsoData #-}
hsoData :: HSO -> [Dynamic]
hsoData  Nil       = []
hsoData (HSO _ ds) = ds

{-# NOINLINE addHSOFinalizer #-}
addHSOFinalizer :: HSO -> IO () -> IO ()
addHSOFinalizer  Nil        = const (return ())
addHSOFinalizer (HSO fp _)  = addForeignPtrFinalizer fp

-- TODO: check return codes
registerHSObjectClass :: Class -> Maybe (IO Dynamic) -> IO ()
registerHSObjectClass cls mbInit = do
    super       <- class_getSuperclass cls
    protoExists <- class_conformsToProtocol super _HSObject_protocol
    if not protoExists
        then do
            let hsInit = maybe (return []) (fmap return) mbInit
            
            __hsInit_IMP <- wrapHsInitIMP (\_ _ -> newStablePtr =<< hsInit)
            class_addMethod cls __hsInit __hsInit_IMP
            
            implementMemoryManagement cls
        else do
            whenJust mbInit $ \hsInit -> do
                __hsInit_IMP <- wrapHsInitIMP $ \self _sel -> do
                    d  <- hsInit
                    ds <- deRefAndFreeStablePtr =<<
                        msgSendSuper (ObjCSuper self super) __hsInit
                    
                    newStablePtr (d:ds)
                
                class_addMethod cls __hsInit __hsInit_IMP
                return ()
    
    objc_registerClassPair cls
    class_addProtocol cls _HSObject_protocol
    
    return ()

implementMemoryManagement :: Class -> IO ()
implementMemoryManagement cls = do
    class_addIvar cls "__hsSelf__"         [nullStablePtr]
    class_addIvar cls "__hsRetainedSelf__" [nullStablePtr]
    
    __hsSelf__ivar         <- class_getInstanceVariable cls "__hsSelf__"
    __hsRetainedSelf__ivar <- class_getInstanceVariable cls "__hsRetainedSelf__"
    
    let getRetainedSelf :: Id -> IO (StablePtr HSO)
        getRetainedSelf self =
            castPtrToStablePtr . castPtr
                <$> object_getIvar self __hsRetainedSelf__ivar
        
        setRetainedSelf :: Id -> StablePtr HSO -> IO ()
        setRetainedSelf self hsSelf = do
            object_setIvar self __hsRetainedSelf__ivar
                (castPtr (castStablePtrToPtr hsSelf))
        
        getHsSelf :: Id -> IO (StablePtr (Weak HSO))
        getHsSelf self = 
            castPtrToStablePtr . castPtr
                <$> object_getIvar self __hsSelf__ivar
        
        setHsSelf :: Id -> StablePtr (Weak HSO) -> IO ()
        setHsSelf self hsSelf =
            object_setIvar self __hsSelf__ivar 
                (castPtr (castStablePtrToPtr hsSelf))
    
    -- this is a bit coarser-grained locking than I'd like,
    -- but it's a very easy way to go and I don't think there
    -- will be much contention in practice.
    classLock <- newMVar ()
    
    super <- class_getSuperclass cls
    
    -- return the "haskell self", initializing it if needed.
    __hsGetSelf_IMP <- wrapHsGetSelfIMP $ \self _sel outIsNew -> do
        withMVar classLock $ \_ -> do
            weakSelf <- getHsSelf self
            
            mbSelf <- if weakSelf /= nullStablePtr
                then deRefStablePtr weakSelf >>= deRefWeak
                else return Nothing
            
            when (weakSelf /= nullStablePtr && isNothing mbSelf) $ do
                hPutStrLn stderr ("ERROR: HSObject " ++ show self
                    ++ " is still alive, but its weak self-reference"
                    ++ " has been collected")
            
            let isNew = toObjCBool (isNothing mbSelf)
            when (outIsNew /= nullPtr) (poke outIsNew isNew)
            
            case mbSelf of
                Just hsSelf -> newStablePtr hsSelf
                Nothing     -> do
                    msgSendSuper (ObjCSuper self super) retain
                    !fp <- newIdForeignPtr self
                    
                    dsptr <- msgSend self __hsInit
                    !ds <- if dsptr /= nullStablePtr
                        then deRefAndFreeStablePtr dsptr
                        else return []
                    
                    hsSelf <- newStablePtr (HSO fp ds)
                    setRetainedSelf self hsSelf
                    
                    setHsSelf self
                        =<< newStablePtr 
                        =<< flip mkWeakPtr Nothing
                        =<< deRefStablePtr hsSelf
                    
                    return hsSelf
    
    -- 'retain': if there is an "hsSelf", retain it
    -- on the Haskell side as well
    retain_IMP <- wrapRetainIMP $ \self retainSel -> do
        msgSendSuper (ObjCSuper self super) retainSel
        
        withMVar classLock $ \_ -> do
            retainedSelf <- getRetainedSelf self
            when (retainedSelf == nullStablePtr) $ do
                weakSelf <- getHsSelf self
                when (weakSelf /= nullStablePtr) $ do
                    mbSelf <- deRefWeak =<< deRefStablePtr weakSelf
                    
                    case mbSelf of
                        Nothing -> hPutStrLn stderr
                            ("ERROR: in -[HSObject retain]: Object " ++ show self
                            ++ " is alive and inited, but its weak self-reference"
                            ++ " has already been collected")
                        Just hsSelf -> do
                            sp <- newStablePtr hsSelf
                            setRetainedSelf self sp
        
        return self
    
    -- 'release': if there is an hsSelf _and_ only one remaining 
    -- reference after the retain, that means that the only reference
    -- remaining is on the Haskell side.  Free the retainedSelf
    -- StablePtr so that it becomes possible for the garbage collector
    -- to free the HSO.
    release_IMP <- wrapVoidIMP $ \self releaseSel -> 
        withMVar classLock $ \_ -> do
            oldRetainCount <- msgSend self retainCount
            msgSendSuper (ObjCSuper self super) releaseSel
            
            let newRetainCount = oldRetainCount - 1
            when (newRetainCount == 1) $ do
                retainedSelf <- getRetainedSelf self
                when (retainedSelf /= nullStablePtr) $ do
                    setRetainedSelf self nullStablePtr
                    freeStablePtr retainedSelf
    
    -- 'dealloc': The last reference has been released.
    -- If 'hsSelf' is not null, free it (even though the Weak ref
    -- will have been zeroed, the weak ref cell itself needs to be
    -- freed).
    dealloc_IMP <- wrapVoidIMP $ \self deallocSel -> do
        hsSelf <- getHsSelf self
        when (hsSelf /= nullStablePtr) $
            freeStablePtr hsSelf
        
        msgSendSuper (ObjCSuper self super) deallocSel
    
    class_addMethod cls __hsGetSelf __hsGetSelf_IMP
    
    class_addMethod cls retain retain_IMP
    class_addMethod cls release release_IMP
    class_addMethod cls dealloc dealloc_IMP
    
    return  ()

importObject :: Ptr ObjCObject -> IO (HSO, Bool)
importObject obj = alloca $ \p -> do
    hso <- importObject' p obj
    isNew <- peek p
    return (hso, fromObjCBool isNew)

importObject_ :: Ptr ObjCObject -> IO HSO
importObject_ = importObject' nullPtr

importObject' :: Ptr CSChar -> Ptr ObjCObject -> IO HSO
importObject' outIsNew obj = do
    when (outIsNew /= nullPtr) (poke outIsNew 1)
    
    cls         <- object_getClass obj
    isHSObject  <- class_conformsToProtocol cls _HSObject_protocol
    
    sptr <- if isHSObject
        then msgSend obj __hsGetSelf outIsNew
        else return nullStablePtr
    
    if sptr /= nullStablePtr
        then deRefAndFreeStablePtr sptr
        else do
            retainObject obj
            fp <- newIdForeignPtr obj
            return $! HSO fp []

deRefAndFreeStablePtr :: StablePtr a -> IO a
deRefAndFreeStablePtr sp = deRefStablePtr sp <* freeStablePtr sp

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = flip (maybe (return ()))

newIdForeignPtr :: Ptr ObjCObject -> IO (ForeignPtr ObjCObject)
newIdForeignPtr = newForeignPtr <*> (unwrapException . finalizeObject)

nullStablePtr :: StablePtr a
nullStablePtr = castPtrToStablePtr nullPtr

{-# NOINLINE _HSObject_protocol #-}
foreign import ccall _HSObject_protocol :: Ptr Protocol

-- selectors and type aliases for convenience

retainCount :: SEL (IO CInt)
retainCount = getSEL "retainCount"

type HsInit = IO (StablePtr [Dynamic])

__hsInit :: SEL HsInit
__hsInit = getSEL "__hsInit"

type HsGetSelf = Ptr CSChar -> IO (StablePtr HSO)

__hsGetSelf :: SEL HsGetSelf
__hsGetSelf = getSEL "__hsGetSelf:"

type Retain = IO (Ptr ObjCObject)

retain :: SEL Retain
retain = getSEL "retain"

type Void = IO ()

release :: SEL Void
release = getSEL "release"

dealloc :: SEL Void
dealloc = getSEL "dealloc"

-- method IMP wrappers

foreign import ccall "wrapper"
    wrapHsInitIMP :: (Ptr ObjCObject -> SEL HsInit -> HsInit) -> IO (IMP HsInit)

foreign import ccall "wrapper"
    wrapHsGetSelfIMP :: (Ptr ObjCObject -> SEL HsGetSelf -> HsGetSelf) -> IO (IMP HsGetSelf)

foreign import ccall "wrapper"
    wrapRetainIMP :: (Ptr ObjCObject -> SEL Retain -> Retain) -> IO (IMP Retain)

foreign import ccall "wrapper"
    wrapVoidIMP :: (Ptr ObjCObject -> SEL Void -> Void) -> IO (IMP Void)

foreign import ccall
    finalizeObject :: Ptr ObjCObject -> Ptr ObjCException -> IO CInt
