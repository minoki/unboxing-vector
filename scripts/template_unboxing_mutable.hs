length :: (Unboxable a) => MVector s a -> Int

null :: (Unboxable a) => MVector s a -> Bool

slice :: (Unboxable a) => Int -> Int -> MVector s a -> MVector s a

init :: (Unboxable a) => MVector s a -> MVector s a

tail :: (Unboxable a) => MVector s a -> MVector s a

take :: (Unboxable a) => Int -> MVector s a -> MVector s a

drop :: (Unboxable a) => Int -> MVector s a -> MVector s a

splitAt :: (Unboxable a) => Int -> MVector s a -> (MVector s a, MVector s a)

unsafeSlice :: (Unboxable a) => Int -> Int -> MVector s a -> MVector s a

unsafeInit :: (Unboxable a) => MVector s a -> MVector s a

unsafeTail :: (Unboxable a) => MVector s a -> MVector s a

unsafeTake :: (Unboxable a) => Int -> MVector s a -> MVector s a

unsafeDrop :: (Unboxable a) => Int -> MVector s a -> MVector s a

overlaps :: (Unboxable a) => MVector s a -> MVector s a -> Bool

new :: (PrimMonad m, Unboxable a) => Int -> m (MVector (PrimState m) a)

unsafeNew :: (PrimMonad m, Unboxable a) => Int -> m (MVector (PrimState m) a)

replicate :: (PrimMonad m, Unboxable a) => Int -> a -> m (MVector (PrimState m) a)

replicateM :: (PrimMonad m, Unboxable a) => Int -> m a -> m (MVector (PrimState m) a)

clone :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> m (MVector (PrimState m) a)

grow :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)

unsafeGrow :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)

clear :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> m ()

read :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Int -> m a

write :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Int -> a -> m ()

modify :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> (a -> a) -> Int -> m ()

swap :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Int -> Int -> m ()

unsafeRead :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Int -> m a

unsafeWrite :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Int -> a -> m ()

unsafeModify :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> (a -> a) -> Int -> m ()

unsafeSwap :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Int -> Int -> m ()

nextPermutation :: (PrimMonad m, Ord e, Unboxable e) => MVector (PrimState m) e -> m Bool

set :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> a -> m ()

copy :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()

move :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()

unsafeCopy :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()

unsafeMove :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
