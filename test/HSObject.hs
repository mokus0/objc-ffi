{-# LANGUAGE ForeignFunctionInterface #-}
module HSObject where

import Control.Monad
import Data.Dynamic
import Foreign.ObjC
import Foreign.ObjC.HSObject
import Foreign.Ptr
import Prelude hiding (init)
import System.Mem

withAutoreleasePool action = do
    _NSAutoreleasePool <- objc_getClass "NSAutoreleasePool"
    pool <- msgSend (castPtr _NSAutoreleasePool) (getSEL "alloc") :: IO Id
    pool <- msgSend pool                         (getSEL "init")  :: IO Id
    
    r <- action
    
    pool <- msgSend pool (getSEL "release") :: IO ()
    
    return r

foreign import ccall "wrapper" 
    wrapInitIMP :: (Ptr ObjCObject -> SEL (IO Id) -> IO Id) -> IO (IMP (IO Id))

main = withAutoreleasePool $ do
    _NSObject <- objc_getClass "NSObject"
    
    foo     <- objc_allocateClassPair _NSObject "Foo" 0
    fooMeta <- object_getClass (castPtr foo)
    
    fooInit_IMP <- wrapInitIMP $ \self _sel -> do
        self <- msgSendSuper (ObjCSuper self _NSObject) _sel
        
        when (self /= nullPtr) $ do
            ivar <- object_getInstanceVariable self "__hsSelf__" nullPtr
            hsSelf <- object_getIvar self ivar
            putStrLn ("hsSelf (in init): " ++ show hsSelf)
        
        return self
    
    class_addMethod foo init fooInit_IMP
    
    let initFoo = do
            putStrLn "Foo initialized"
            return $! toDyn (42 :: Integer)
    
    registerHSObjectClass foo (Just initFoo)
    
    putStrLn "Registered Foo class"
    
    bar <- msgSend (castPtr foo) alloc
    putStrLn ("allocated bar: " ++ show bar)
    
    bar <- msgSend bar init
    putStrLn ("inited bar: " ++ show bar)
    
    do
        HSO fp ds <- importObject bar
        putStrLn ("imported bar: " ++ show fp ++ " (" ++ show (length ds) ++ " Dynamic(s))")
    
    performGC
    
    putStrLn "performed GC"
    
    do
        HSO fp ds <- importObject bar
        putStrLn ("imported bar again: " ++ show fp ++ " (" ++ show (length ds) ++ " Dynamic(s))")
    
    when True $ do
        performGC
        putStrLn "performed GC again"
    
    msgSend bar release
    putStrLn "released bar"
    
    performGC
    putStrLn "performed GC again"

alloc :: SEL (IO (Ptr ObjCObject))
alloc = getSEL "alloc"

init :: SEL (IO (Ptr ObjCObject))
init = getSEL "init"

release :: SEL (IO ())
release = getSEL "release"