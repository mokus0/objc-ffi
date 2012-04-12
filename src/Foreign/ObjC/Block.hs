{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Foreign.ObjC.Block where

import Control.Monad.State
import Data.Bits
import Data.Int
import Data.Word
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

newtype BlockPtr a = BlockPtr (Ptr (Block a))
    deriving (Eq, Show)

newtype BlockFlags = BlockFlags Int32
    deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Bits, Storable)

hasCopyDisposeFlagBit   = 25 :: Int
hasConstructorFlagBit   = 26 :: Int
isGlobalFlagBit         = 28 :: Int
hasStretFlagBit         = 29 :: Int
hasSignatureFlagBit     = 30 :: Int

data Block a = Block
    { isa           :: Ptr ()
    , flags         :: BlockFlags
    , reserved      :: Int32
    , invoke        :: FunPtr (BlockPtr a -> a)
    , descriptor    :: Ptr (BlockDescriptor a)
    } deriving (Eq, Show)

instance Storable (Block a) where
    sizeOf    _ = 0x20
    alignment _ = 0x08
    peek p = do
        isa         <- peekByteOff p 0x00
        flags       <- peekByteOff p 0x08
        reserved    <- peekByteOff p 0x0c
        invoke      <- peekByteOff p 0x10
        descriptor  <- peekByteOff p 0x18
        return Block{..}
    poke p Block{..} = do
        pokeByteOff p 0x00 isa
        pokeByteOff p 0x08 flags
        pokeByteOff p 0x0c reserved
        pokeByteOff p 0x10 invoke
        pokeByteOff p 0x18 descriptor

-- NOTE: The actual layout of this structure depends on the 'flags' field
-- of the block it is attached to.  See:
--
-- http://clang.llvm.org/docs/Block-ABI-Apple.txt
data BlockDescriptor a = BlockDescriptor
    { reserved2     :: Word64
    , size          :: Word64
    , copyHelper    :: FunPtr (BlockPtr a -> BlockPtr a -> IO ())
    , disposeHelper :: FunPtr (BlockPtr a -> IO ())
    , signature     :: CString
    } deriving (Eq, Show)

nextPtr :: (Monad m, Storable t) => StateT (Ptr a) m (Ptr t)
nextPtr = do
    p <- liftM castPtr get 
    let proxy = (undefined :: t a -> a) p
        p' = alignPtr p (alignment proxy)
    put $! plusPtr p' (sizeOf proxy)
    return p'

sizeOfBlockDescriptor :: BlockFlags -> Int
sizeOfBlockDescriptor flags = endPtr `minusPtr` nullPtr
    where
        endPtr = execState (peekBlockDescriptorWith (nextPtr >>= fakePeek) flags) nullPtr
        fakePeek :: Ptr t -> State s t
        fakePeek _ = return undefined

peekBlockDescriptor :: BlockFlags -> Ptr (BlockDescriptor a) -> IO (BlockDescriptor a)
peekBlockDescriptor = evalStateT . peekBlockDescriptorWith (lift . peek =<< nextPtr)

pokeBlockDescriptor :: BlockFlags -> Ptr (BlockDescriptor a) -> BlockDescriptor a -> IO ()
pokeBlockDescriptor flags = flip (evalStateT . pokeBlockDescriptorWith pokeNext flags)
    where pokeNext it = lift . flip poke it =<< nextPtr

peekBlockDescriptorWith :: Monad m =>
       (forall t. Storable t => m t)
    -> BlockFlags -> m (BlockDescriptor a)
peekBlockDescriptorWith peekNext flags = do
    reserved2       <- peekNext
    size            <- peekNext
    copyHelper      <- if testBit flags hasCopyDisposeFlagBit
        then peekNext
        else return nullFunPtr
    disposeHelper   <- if testBit flags hasCopyDisposeFlagBit
        then peekNext
        else return nullFunPtr
    signature       <- if testBit flags hasSignatureFlagBit
        then peekNext
        else return nullPtr
    return BlockDescriptor{..}

pokeBlockDescriptorWith :: Monad m => 
       (forall t. Storable t => t -> m ())
    -> BlockFlags -> BlockDescriptor a -> m ()
pokeBlockDescriptorWith pokeNext flags BlockDescriptor{..} = do
    pokeNext reserved2
    pokeNext size
    when (testBit flags hasCopyDisposeFlagBit) $ do
        pokeNext copyHelper
        pokeNext disposeHelper
    when (testBit flags hasSignatureFlagBit) $ do
        pokeNext signature

getBlockDescriptor :: Block a -> IO (BlockDescriptor a)
getBlockDescriptor blk = peekBlockDescriptor (flags blk) (descriptor blk)

-- |Sets BLOCK_HAS_COPY_DISPOSE and BLOCK_HAS_SIGNATURE flags appropriately
-- for the given block descriptor.  Note that this does not even try to
-- determine whether any other flags should be set.
descriptorFormatFlags :: BlockDescriptor a -> BlockFlags
descriptorFormatFlags BlockDescriptor{..} = foldr (.|.) 0 $ concat
    [ [ bit hasCopyDisposeFlagBit | copyHelper    /= nullFunPtr ]
    , [ bit hasCopyDisposeFlagBit | disposeHelper /= nullFunPtr ]
    , [ bit hasSignatureFlagBit   | signature     /= nullPtr    ]
    ]
