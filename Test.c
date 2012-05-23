

// foreign import ccall unsafe "lotsa" unsafeLotsa :: Ptr Word8 -> Word64 -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO Bool

// foreign import ccall safe "justOne" safeJustOne :: Ptr Word8 -> Ptr JustOne -> IO Bool

int lotsa()
{
    return 1;
}

int justOne()
{
    return 1;
}

