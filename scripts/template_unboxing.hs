length :: (Unboxable a) => Vector a -> Int

null :: (Unboxable a) => Vector a -> Bool

(!) :: (Unboxable a) => Vector a -> Int -> a

(!?) :: (Unboxable a) => Vector a -> Int -> Maybe a

head :: (Unboxable a) => Vector a -> a

last :: (Unboxable a) => Vector a -> a

unsafeIndex :: (Unboxable a) => Vector a -> Int -> a

unsafeHead :: (Unboxable a) => Vector a -> a

unsafeLast :: (Unboxable a) => Vector a -> a

indexM :: (Monad m, Unboxable a) => Vector a -> Int -> m a

headM :: (Monad m, Unboxable a) => Vector a -> m a

lastM :: (Monad m, Unboxable a) => Vector a -> m a

unsafeIndexM :: (Monad m, Unboxable a) => Vector a -> Int -> m a

unsafeHeadM :: (Monad m, Unboxable a) => Vector a -> m a

unsafeLastM :: (Monad m, Unboxable a) => Vector a -> m a

slice :: (Unboxable a) => Int -> Int -> Vector a -> Vector a

init :: (Unboxable a) => Vector a -> Vector a

tail :: (Unboxable a) => Vector a -> Vector a

take :: (Unboxable a) => Int -> Vector a -> Vector a

drop :: (Unboxable a) => Int -> Vector a -> Vector a

splitAt :: (Unboxable a) => Int -> Vector a -> (Vector a, Vector a)

unsafeSlice :: (Unboxable a) => Int -> Int -> Vector a -> Vector a

unsafeInit :: (Unboxable a) => Vector a -> Vector a

unsafeTail :: (Unboxable a) => Vector a -> Vector a

unsafeTake :: (Unboxable a) => Int -> Vector a -> Vector a

unsafeDrop :: (Unboxable a) => Int -> Vector a -> Vector a

empty :: (Unboxable a) => Vector a

singleton :: (Unboxable a) => a -> Vector a

replicate :: (Unboxable a) => Int -> a -> Vector a

generate :: (Unboxable a) => Int -> (Int -> a) -> Vector a

iterateN :: (Unboxable a) => Int -> (a -> a) -> a -> Vector a

replicateM :: (Monad m, Unboxable a) => Int -> m a -> m (Vector a)

generateM :: (Monad m, Unboxable a) => Int -> (Int -> m a) -> m (Vector a)

iterateNM :: (Monad m, Unboxable a) => Int -> (a -> m a) -> a -> m (Vector a)

create :: (Unboxable a) => (forall s. ST s (MVector s a)) -> Vector a

createT :: (Traversable f, Unboxable a) => (forall s. ST s (f (MVector s a))) -> f (Vector a)

unfoldr :: (Unboxable a) => (b -> Maybe (a, b)) -> b -> Vector a

unfoldrN :: (Unboxable a) => Int -> (b -> Maybe (a, b)) -> b -> Vector a

unfoldrM :: (Monad m, Unboxable a) => (b -> m (Maybe (a, b))) -> b -> m (Vector a)

unfoldrNM :: (Monad m, Unboxable a) => Int -> (b -> m (Maybe (a, b))) -> b -> m (Vector a)

constructN :: (Unboxable a) => Int -> (Vector a -> a) -> Vector a

constructrN :: (Unboxable a) => Int -> (Vector a -> a) -> Vector a

enumFromN :: (Num a, Unboxable a) => a -> Int -> Vector a

enumFromStepN :: (Num a, Unboxable a) => a -> a -> Int -> Vector a

enumFromTo :: (Enum a, Unboxable a) => a -> a -> Vector a

enumFromThenTo :: (Enum a, Unboxable a) => a -> a -> a -> Vector a

cons :: (Unboxable a) => a -> Vector a -> Vector a

snoc :: (Unboxable a) => Vector a -> a -> Vector a

(++) :: (Unboxable a) => Vector a -> Vector a -> Vector a

concat :: (Unboxable a) => [Vector a] -> Vector a

force :: (Unboxable a) => Vector a -> Vector a

(//) :: (Unboxable a) => Vector a -> [(Int, a)] -> Vector a

update :: (Unboxable a) => Vector a -> Vector (Int, a) -> Vector a

update_ :: (Unboxable a) => Vector a -> Vector Int -> Vector a -> Vector a

unsafeUpd :: (Unboxable a) => Vector a -> [(Int, a)] -> Vector a

unsafeUpdate :: (Unboxable a) => Vector a -> Vector (Int, a) -> Vector a

unsafeUpdate_ :: (Unboxable a) => Vector a -> Vector Int -> Vector a -> Vector a

accum :: (Unboxable a) => (a -> b -> a) -> Vector a -> [(Int, b)] -> Vector a

accumulate :: (Unboxable a, Unboxable b) => (a -> b -> a) -> Vector a -> Vector (Int, b) -> Vector a

accumulate_ :: (Unboxable a, Unboxable b) => (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a

unsafeAccum :: (Unboxable a) => (a -> b -> a) -> Vector a -> [(Int, b)] -> Vector a

unsafeAccumulate :: (Unboxable a, Unboxable b) => (a -> b -> a) -> Vector a -> Vector (Int, b) -> Vector a

unsafeAccumulate_ :: (Unboxable a, Unboxable b) => (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a

reverse :: (Unboxable a) => Vector a -> Vector a

backpermute :: (Unboxable a) => Vector a -> Vector Int -> Vector a

unsafeBackpermute :: (Unboxable a) => Vector a -> Vector Int -> Vector a

modify :: (Unboxable a) => (forall s. MVector s a -> ST s ()) -> Vector a -> Vector a

indexed :: (Unboxable a) => Vector a -> Vector (Int, a)

map :: (Unboxable a, Unboxable b) => (a -> b) -> Vector a -> Vector b

imap :: (Unboxable a, Unboxable b) => (Int -> a -> b) -> Vector a -> Vector b

concatMap :: (Unboxable a, Unboxable b) => (a -> Vector b) -> Vector a -> Vector b

mapM :: (Monad m, Unboxable a, Unboxable b) => (a -> m b) -> Vector a -> m (Vector b)

imapM :: (Monad m, Unboxable a, Unboxable b) => (Int -> a -> m b) -> Vector a -> m (Vector b)

mapM_ :: (Monad m, Unboxable a) => (a -> m b) -> Vector a -> m ()

imapM_ :: (Monad m, Unboxable a) => (Int -> a -> m b) -> Vector a -> m ()

forM :: (Monad m, Unboxable a, Unboxable b) => Vector a -> (a -> m b) -> m (Vector b)

forM_ :: (Monad m, Unboxable a) => Vector a -> (a -> m b) -> m ()

zipWith :: (Unboxable a, Unboxable b, Unboxable c) => (a -> b -> c) -> Vector a -> Vector b -> Vector c

zipWith3 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d) => (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d

zipWith4 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e) => (a -> b -> c -> d -> e) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e

zipWith5 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e, Unboxable f) => (a -> b -> c -> d -> e -> f) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f

zipWith6 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e, Unboxable f, Unboxable g) => (a -> b -> c -> d -> e -> f -> g) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f -> Vector g

izipWith :: (Unboxable a, Unboxable b, Unboxable c) => (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c

izipWith3 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d) => (Int -> a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d

izipWith4 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e) => (Int -> a -> b -> c -> d -> e) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e

izipWith5 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e, Unboxable f) => (Int -> a -> b -> c -> d -> e -> f) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f

izipWith6 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e, Unboxable f, Unboxable g) => (Int -> a -> b -> c -> d -> e -> f -> g) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f -> Vector g

zip :: (Unboxable a, Unboxable b) => Vector a -> Vector b -> Vector (a, b)

zip3 :: (Unboxable a, Unboxable b, Unboxable c) => Vector a -> Vector b -> Vector c -> Vector (a, b, c)

zip4 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d) => Vector a -> Vector b -> Vector c -> Vector d -> Vector (a, b, c, d)

zip5 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e) => Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector (a, b, c, d, e)

zip6 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e, Unboxable f) => Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f -> Vector (a, b, c, d, e, f)

zipWithM :: (Monad m, Unboxable a, Unboxable b, Unboxable c) => (a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)

izipWithM :: (Monad m, Unboxable a, Unboxable b, Unboxable c) => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)

zipWithM_ :: (Monad m, Unboxable a, Unboxable b) => (a -> b -> m c) -> Vector a -> Vector b -> m ()

izipWithM_ :: (Monad m, Unboxable a, Unboxable b) => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m ()

unzip :: (Unboxable a, Unboxable b) => Vector (a, b) -> (Vector a, Vector b)

unzip3 :: (Unboxable a, Unboxable b, Unboxable c) => Vector (a, b, c) -> (Vector a, Vector b, Vector c)

unzip4 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d) => Vector (a, b, c, d) -> (Vector a, Vector b, Vector c, Vector d)

unzip5 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e) => Vector (a, b, c, d, e) -> (Vector a, Vector b, Vector c, Vector d, Vector e)

unzip6 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e, Unboxable f) => Vector (a, b, c, d, e, f) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f)

filter :: (Unboxable a) => (a -> Bool) -> Vector a -> Vector a

ifilter :: (Unboxable a) => (Int -> a -> Bool) -> Vector a -> Vector a

uniq :: (Eq a, Unboxable a) => Vector a -> Vector a

mapMaybe :: (Unboxable a, Unboxable b) => (a -> Maybe b) -> Vector a -> Vector b

imapMaybe :: (Unboxable a, Unboxable b) => (Int -> a -> Maybe b) -> Vector a -> Vector b

filterM :: (Monad m, Unboxable a) => (a -> m Bool) -> Vector a -> m (Vector a)

takeWhile :: (Unboxable a) => (a -> Bool) -> Vector a -> Vector a

dropWhile :: (Unboxable a) => (a -> Bool) -> Vector a -> Vector a

partition :: (Unboxable a) => (a -> Bool) -> Vector a -> (Vector a, Vector a)

unstablePartition :: (Unboxable a) => (a -> Bool) -> Vector a -> (Vector a, Vector a)

span :: (Unboxable a) => (a -> Bool) -> Vector a -> (Vector a, Vector a)

break :: (Unboxable a) => (a -> Bool) -> Vector a -> (Vector a, Vector a)

elem :: (Eq a, Unboxable a) => a -> Vector a -> Bool

notElem :: (Eq a, Unboxable a) => a -> Vector a -> Bool

find :: (Unboxable a) => (a -> Bool) -> Vector a -> Maybe a

findIndex :: (Unboxable a) => (a -> Bool) -> Vector a -> Maybe Int

findIndices :: (Unboxable a) => (a -> Bool) -> Vector a -> Vector Int

elemIndex :: (Eq a, Unboxable a) => a -> Vector a -> Maybe Int

elemIndices :: (Eq a, Unboxable a) => a -> Vector a -> Vector Int

foldl :: (Unboxable b) => (a -> b -> a) -> a -> Vector b -> a

foldl1 :: (Unboxable a) => (a -> a -> a) -> Vector a -> a

foldl' :: (Unboxable b) => (a -> b -> a) -> a -> Vector b -> a

foldl1' :: (Unboxable a) => (a -> a -> a) -> Vector a -> a

foldr :: (Unboxable a) => (a -> b -> b) -> b -> Vector a -> b

foldr1 :: (Unboxable a) => (a -> a -> a) -> Vector a -> a

foldr' :: (Unboxable a) => (a -> b -> b) -> b -> Vector a -> b

foldr1' :: (Unboxable a) => (a -> a -> a) -> Vector a -> a

ifoldl :: (Unboxable b) => (a -> Int -> b -> a) -> a -> Vector b -> a

ifoldl' :: (Unboxable b) => (a -> Int -> b -> a) -> a -> Vector b -> a

ifoldr :: (Unboxable a) => (Int -> a -> b -> b) -> b -> Vector a -> b

ifoldr' :: (Unboxable a) => (Int -> a -> b -> b) -> b -> Vector a -> b

all :: (Unboxable a) => (a -> Bool) -> Vector a -> Bool

any :: (Unboxable a) => (a -> Bool) -> Vector a -> Bool

and :: Vector Bool -> Bool

or :: Vector Bool -> Bool

sum :: (Num a, Unboxable a) => Vector a -> a

product :: (Num a, Unboxable a) => Vector a -> a

maximum :: (Ord a, Unboxable a) => Vector a -> a

maximumBy :: (Unboxable a) => (a -> a -> Ordering) -> Vector a -> a

minimum :: (Ord a, Unboxable a) => Vector a -> a

minimumBy :: (Unboxable a) => (a -> a -> Ordering) -> Vector a -> a

minIndex :: (Ord a, Unboxable a) => Vector a -> Int

minIndexBy :: (Unboxable a) => (a -> a -> Ordering) -> Vector a -> Int

maxIndex :: (Ord a, Unboxable a) => Vector a -> Int

maxIndexBy :: (Unboxable a) => (a -> a -> Ordering) -> Vector a -> Int

foldM :: (Monad m, Unboxable b) => (a -> b -> m a) -> a -> Vector b -> m a

ifoldM :: (Monad m, Unboxable b) => (a -> Int -> b -> m a) -> a -> Vector b -> m a

foldM' :: (Monad m, Unboxable b) => (a -> b -> m a) -> a -> Vector b -> m a

ifoldM' :: (Monad m, Unboxable b) => (a -> Int -> b -> m a) -> a -> Vector b -> m a

fold1M :: (Monad m, Unboxable a) => (a -> a -> m a) -> Vector a -> m a

fold1M' :: (Monad m, Unboxable a) => (a -> a -> m a) -> Vector a -> m a

foldM_ :: (Monad m, Unboxable b) => (a -> b -> m a) -> a -> Vector b -> m ()

ifoldM_ :: (Monad m, Unboxable b) => (a -> Int -> b -> m a) -> a -> Vector b -> m ()

foldM'_ :: (Monad m, Unboxable b) => (a -> b -> m a) -> a -> Vector b -> m ()

ifoldM'_ :: (Monad m, Unboxable b) => (a -> Int -> b -> m a) -> a -> Vector b -> m ()

fold1M_ :: (Monad m, Unboxable a) => (a -> a -> m a) -> Vector a -> m ()

fold1M'_ :: (Monad m, Unboxable a) => (a -> a -> m a) -> Vector a -> m ()

prescanl :: (Unboxable a, Unboxable b) => (a -> b -> a) -> a -> Vector b -> Vector a

prescanl' :: (Unboxable a, Unboxable b) => (a -> b -> a) -> a -> Vector b -> Vector a

postscanl :: (Unboxable a, Unboxable b) => (a -> b -> a) -> a -> Vector b -> Vector a

postscanl' :: (Unboxable a, Unboxable b) => (a -> b -> a) -> a -> Vector b -> Vector a

scanl :: (Unboxable a, Unboxable b) => (a -> b -> a) -> a -> Vector b -> Vector a

scanl' :: (Unboxable a, Unboxable b) => (a -> b -> a) -> a -> Vector b -> Vector a

scanl1 :: (Unboxable a) => (a -> a -> a) -> Vector a -> Vector a

scanl1' :: (Unboxable a) => (a -> a -> a) -> Vector a -> Vector a

iscanl :: (Unboxable a, Unboxable b) => (Int -> a -> b -> a) -> a -> Vector b -> Vector a

iscanl' :: (Unboxable a, Unboxable b) => (Int -> a -> b -> a) -> a -> Vector b -> Vector a

prescanr :: (Unboxable a, Unboxable b) => (a -> b -> b) -> b -> Vector a -> Vector b

prescanr' :: (Unboxable a, Unboxable b) => (a -> b -> b) -> b -> Vector a -> Vector b

postscanr :: (Unboxable a, Unboxable b) => (a -> b -> b) -> b -> Vector a -> Vector b

postscanr' :: (Unboxable a, Unboxable b) => (a -> b -> b) -> b -> Vector a -> Vector b

scanr :: (Unboxable a, Unboxable b) => (a -> b -> b) -> b -> Vector a -> Vector b

scanr' :: (Unboxable a, Unboxable b) => (a -> b -> b) -> b -> Vector a -> Vector b

scanr1 :: (Unboxable a) => (a -> a -> a) -> Vector a -> Vector a

scanr1' :: (Unboxable a) => (a -> a -> a) -> Vector a -> Vector a

iscanr :: (Unboxable a, Unboxable b) => (Int -> a -> b -> b) -> b -> Vector a -> Vector b

iscanr' :: (Unboxable a, Unboxable b) => (Int -> a -> b -> b) -> b -> Vector a -> Vector b

toList :: (Unboxable a) => Vector a -> [a]

fromList :: (Unboxable a) => [a] -> Vector a

fromListN :: (Unboxable a) => Int -> [a] -> Vector a

freeze :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> m (Vector a)

thaw :: (PrimMonad m, Unboxable a) => Vector a -> m (MVector (PrimState m) a)

copy :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Vector a -> m ()

unsafeFreeze :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> m (Vector a)

unsafeThaw :: (PrimMonad m, Unboxable a) => Vector a -> m (MVector (PrimState m) a)

unsafeCopy :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Vector a -> m ()
