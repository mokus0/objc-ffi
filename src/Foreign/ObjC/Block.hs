{-# LANGUAGE RecordWildCards #-}
-- |WARNING: this code is neither complete nor correct.
module Foreign.ObjC.Block where

import Data.Int
import Data.Word
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

newtype BlockPtr a = BlockPtr (Ptr (Block a))
    deriving (Eq, Show)

data Block a = Block
    { isa           :: Ptr ()
    , flags         :: Int32
    , reserved      :: Int32
    , invoke        :: FunPtr (BlockPtr a -> a)
    , descriptor    :: Ptr BlockDescriptor
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

-- NOTE: I'm seeing conflicting reports on the net about this
-- structure.  For example:
--
-- http://hackage.haskell.org/trac/ghc/wiki/BlockObjects/FakingIt
-- http://cocoawithlove.com/2009/10/how-blocks-are-implemented-and.html
-- http://clang.llvm.org/docs/Block-ABI-Apple.txt
--
-- https://github.com/mikeash/MABlockClosure/blob/master/README.markdown
--   suggests that the implementation is different between GCC and CLANG...
data BlockDescriptor = BlockDescriptor
    { reserved2     :: Word64
    , size          :: Word64
    , signature     :: CString
    , undocumented  :: Word64
    } deriving (Eq, Show)

instance Storable BlockDescriptor where
    sizeOf    _ = 0x20
    alignment _ = 0x08
    peek p = do
        reserved2       <- peekByteOff p 0x00
        size            <- peekByteOff p 0x08
        signature       <- peekByteOff p 0x10
        undocumented    <- peekByteOff p 0x18
        return BlockDescriptor{..}
    poke p BlockDescriptor{..} = do
        pokeByteOff p 0x00 reserved2
        pokeByteOff p 0x08 size
        pokeByteOff p 0x10 signature
        pokeByteOff p 0x18 undocumented
