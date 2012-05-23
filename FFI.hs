module FFI where

import Data.Word
import Foreign.Ptr
import Foreign.Storable

foreign import ccall safe "lotsa" safeLotsa :: Ptr Word8 -> Word64 -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO Bool
foreign import ccall unsafe "lotsa" unsafeLotsa :: Ptr Word8 -> Word64 -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO Bool

foreign import ccall safe "justOne" safeJustOne :: Ptr Word8 -> Word64 -> Ptr JustOne -> IO Bool
foreign import ccall unsafe "justOne" unsafeJustOne :: Ptr Word8 -> Word64 -> Ptr JustOne -> IO Bool

data JustOne = JustOne {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64

instance Storable JustOne where
  sizeOf _ = 40
  alignment _ = 8
  poke ptr val = return ()
  peek ptr = do
    x1 <- peek (plusPtr ptr 0x00)
    x2 <- peek (plusPtr ptr 0x08)
    x3 <- peek (plusPtr ptr 0x10)
    x4 <- peek (plusPtr ptr 0x18)
    x5 <- peek (plusPtr ptr 0x20)
    return $! JustOne x1 x2 x3 x4 x5

