{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Vector.Unboxing.Generic.Internal where
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Fusion.Bundle as Bundle
import qualified GHC.Generics
import Data.Coerce
import Data.Semigroup
import Data.Int
import Data.Word
import qualified Data.Complex
import qualified Data.Functor.Identity
import qualified Data.Functor.Const
import qualified Data.Ord
import qualified Data.Monoid
import qualified Data.MonoTraversable -- from mono-traversable
import qualified Data.Sequences       -- from mono-traversable
import GHC.Exts (IsList(..))
import Control.DeepSeq (NFData(..))
import Text.Read (Read(..),readListPrecDefault)
import GHC.TypeLits (TypeError,ErrorMessage(Text))

class (U.Unbox (Rep a)) => Unboxable a where
  type Rep a
  from :: a -> Rep a
  to :: Rep a -> a

  -- Default definition using GHC.Generics:
  type Rep a = Rep' (GHC.Generics.Rep a) -- needs UndecidableInstances here
  default from :: (GHC.Generics.Generic a, Unboxable' (GHC.Generics.Rep a), Rep a ~ Rep' (GHC.Generics.Rep a)) => a -> Rep a
  default to :: (GHC.Generics.Generic a, Unboxable' (GHC.Generics.Rep a), Rep a ~ Rep' (GHC.Generics.Rep a)) => Rep a -> a
  from = from' . GHC.Generics.from
  to = GHC.Generics.to . to'
  {-# INLINE from #-}
  {-# INLINE to #-}

class Unboxable' f where
  type Rep' f
  from' :: f x -> Rep' f
  to' :: Rep' f -> f x
instance Unboxable' GHC.Generics.U1 where
  type Rep' GHC.Generics.U1 = ()
  from' _ = ()
  to' _ = GHC.Generics.U1
  {-# INLINE from' #-}
  {-# INLINE to' #-}
instance Unboxable c => Unboxable' (GHC.Generics.K1 i c) where
  type Rep' (GHC.Generics.K1 i c) = Rep c
  from' = {- from . GHC.Generics.unK1 -} coerce (from :: c -> Rep c)
  to' = {- GHC.Generics.K1 . to -} coerce (to :: Rep c -> c)
  {-# INLINE from' #-}
  {-# INLINE to' #-}
instance Unboxable' f => Unboxable' (GHC.Generics.M1 i c f) where
  type Rep' (GHC.Generics.M1 i c f) = Rep' f
  from' = from' . GHC.Generics.unM1
  to' = GHC.Generics.M1 . to'
  {-# INLINE from' #-}
  {-# INLINE to' #-}
instance (Unboxable' f, Unboxable' g) => Unboxable' (f GHC.Generics.:*: g) where
  type Rep' (f GHC.Generics.:*: g) = (Rep' f, Rep' g)
  from' (x GHC.Generics.:*: y) = (from' x, from' y)
  to' (x, y) = (to' x GHC.Generics.:*: to' y)
  {-# INLINE from' #-}
  {-# INLINE to' #-}
instance (TypeError ('Text "Cannot derive Unboxable instance for a sum type.")) => Unboxable' (f GHC.Generics.:+: g) where
  type Rep' (f GHC.Generics.:+: g) = ()
  from' = undefined
  to' = undefined

newtype Vector a = UnboxingVector (U.Vector (Rep a))
newtype MVector s a = UnboxingMVector (UM.MVector s (Rep a))

type instance G.Mutable Vector = MVector

instance (Unboxable a) => IsList (Vector a) where
  type Item (Vector a) = a
  fromList = G.fromList
  fromListN = G.fromListN
  toList = G.toList
  {-# INLINE fromList #-}
  {-# INLINE fromListN #-}
  {-# INLINE toList #-}

instance (Eq a, Unboxable a) => Eq (Vector a) where
  xs == ys = Bundle.eq (G.stream xs) (G.stream ys)
  xs /= ys = not (Bundle.eq (G.stream xs) (G.stream ys))
  {-# INLINE (==) #-}
  {-# INLINE (/=) #-}

instance (Ord a, Unboxable a) => Ord (Vector a) where
  compare xs ys = Bundle.cmp (G.stream xs) (G.stream ys)
  {-# INLINE compare #-}

instance (Show a, Unboxable a) => Show (Vector a) where
  showsPrec = G.showsPrec
  {-# INLINE showsPrec #-}
instance (Read a, Unboxable a) => Read (Vector a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault
  {-# INLINE readPrec #-}
  {-# INLINE readListPrec #-}

instance (Unboxable a) => Semigroup (Vector a) where
  (<>) = (G.++)
  sconcat = G.concatNE
  {-# INLINE (<>) #-}
  {-# INLINE sconcat #-}

instance (Unboxable a) => Monoid (Vector a) where
  mempty = G.empty
  mappend = (<>)
  mconcat = G.concat
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}
  {-# INLINE mconcat #-}

instance NFData (Vector a) where
  rnf !_ = () -- the content is unboxed

instance (Unboxable a) => GM.MVector MVector a where
  basicLength (UnboxingMVector mv)                           = GM.basicLength mv
  basicUnsafeSlice i l (UnboxingMVector mv)                  = UnboxingMVector (GM.basicUnsafeSlice i l mv)
  basicOverlaps (UnboxingMVector mv) (UnboxingMVector mv')   = GM.basicOverlaps mv mv'
  basicUnsafeNew l                                           = UnboxingMVector <$> GM.basicUnsafeNew l
  basicInitialize (UnboxingMVector mv)                       = GM.basicInitialize mv
  basicUnsafeReplicate i x                                   = UnboxingMVector <$> GM.basicUnsafeReplicate i (from x)
  basicUnsafeRead (UnboxingMVector mv) i                     = to <$> GM.basicUnsafeRead mv i
  basicUnsafeWrite (UnboxingMVector mv) i x                  = GM.basicUnsafeWrite mv i (from x)
  basicClear (UnboxingMVector mv)                            = GM.basicClear mv
  basicSet (UnboxingMVector mv) x                            = GM.basicSet mv (from x)
  basicUnsafeCopy (UnboxingMVector mv) (UnboxingMVector mv') = GM.basicUnsafeCopy mv mv'
  basicUnsafeMove (UnboxingMVector mv) (UnboxingMVector mv') = GM.basicUnsafeMove mv mv'
  basicUnsafeGrow (UnboxingMVector mv) n                     = UnboxingMVector <$> GM.basicUnsafeGrow mv n
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeMove #-}
  {-# INLINE basicUnsafeGrow #-}

instance (Unboxable a) => G.Vector Vector a where
  basicUnsafeFreeze (UnboxingMVector mv)                  = UnboxingVector <$> G.basicUnsafeFreeze mv
  basicUnsafeThaw (UnboxingVector v)                      = UnboxingMVector <$> G.basicUnsafeThaw v
  basicLength (UnboxingVector v)                          = G.basicLength v
  basicUnsafeSlice i l (UnboxingVector v)                 = UnboxingVector (G.basicUnsafeSlice i l v)
  basicUnsafeIndexM (UnboxingVector v) i                  = to <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (UnboxingMVector mv) (UnboxingVector v) = G.basicUnsafeCopy mv v
  elemseq (UnboxingVector v) x y                          = G.elemseq v (from x) y -- ?
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE elemseq #-}

-----

-- Classes from mono-traversable

type instance Data.MonoTraversable.Element (Vector a) = a

instance (Unboxable a) => Data.MonoTraversable.MonoFunctor (Vector a) where
  omap = G.map
  {-# INLINE omap #-}

instance (Unboxable a) => Data.MonoTraversable.MonoFoldable (Vector a) where
  ofoldMap f = G.foldr (mappend . f) mempty
  ofoldr = G.foldr
  ofoldl' = G.foldl'
  otoList = G.toList
  oall = G.all
  oany = G.any
  onull = G.null
  olength = G.length
  olength64 = fromIntegral . G.length
  -- ocompareLength : use default
  -- otraverse_ : use default
  -- ofor_ : use default
  -- omapM_ : use default (G.mapM_ requires a Monad, unfortunately)
  -- oforM_ : use default (G.forM_ requires a Monad, unfortunately)
  ofoldlM = G.foldM
  -- ofoldMap1Ex : use default
  ofoldr1Ex = G.foldr1
  ofoldl1Ex' = G.foldl1'
  headEx = G.head
  lastEx = G.last
  unsafeHead = G.unsafeHead
  unsafeLast = G.unsafeLast
  maximumByEx = G.maximumBy
  minimumByEx = G.minimumBy
  oelem = G.elem
  onotElem = G.notElem
  {-# INLINE ofoldMap #-}
  {-# INLINE ofoldr #-}
  {-# INLINE ofoldl' #-}
  {-# INLINE otoList #-}
  {-# INLINE oall #-}
  {-# INLINE oany #-}
  {-# INLINE onull #-}
  {-# INLINE olength #-}
  {-# INLINE olength64 #-}
  {-# INLINE ofoldlM #-}
  {-# INLINE ofoldr1Ex #-}
  {-# INLINE ofoldl1Ex' #-}
  {-# INLINE headEx #-}
  {-# INLINE lastEx #-}
  {-# INLINE unsafeHead #-}
  {-# INLINE unsafeLast #-}
  {-# INLINE maximumByEx #-}
  {-# INLINE minimumByEx #-}
  {-# INLINE oelem #-}
  {-# INLINE onotElem #-}

instance (Unboxable a) => Data.MonoTraversable.MonoTraversable (Vector a) where
  otraverse f v = let !n = G.length v
                  in G.fromListN n <$> traverse f (G.toList v)
  omapM = Data.MonoTraversable.otraverse
  {-# INLINE otraverse #-}
  {-# INLINE omapM #-}

instance (Unboxable a) => Data.MonoTraversable.MonoPointed (Vector a) where
  opoint = G.singleton
  {-# INLINE opoint #-}

instance (Unboxable a) => Data.MonoTraversable.GrowingAppend (Vector a)

instance (Unboxable a) => Data.Sequences.SemiSequence (Vector a) where
  type Index (Vector a) = Int
  intersperse = Data.Sequences.defaultIntersperse
  reverse = G.reverse
  find = G.find
  sortBy = Data.Sequences.vectorSortBy
  cons = G.cons
  snoc = G.snoc
  {-# INLINE intersperse #-}
  {-# INLINE reverse #-}
  {-# INLINE find #-}
  {-# INLINE sortBy #-}
  {-# INLINE cons #-}
  {-# INLINE snoc #-}

instance (Unboxable a) => Data.Sequences.IsSequence (Vector a) where
  fromList = G.fromList
  lengthIndex = G.length
  break = G.break
  span = G.span
  dropWhile = G.dropWhile
  takeWhile = G.takeWhile
  splitAt = G.splitAt
  -- unsafeSplitAt : use default
  take = G.take
  unsafeTake = G.unsafeTake
  drop = G.drop
  unsafeDrop = G.unsafeDrop
  -- dropEnd : use default
  partition = G.partition
  uncons v | G.null v = Nothing
           | otherwise = Just (G.head v, G.tail v)
  unsnoc v | G.null v = Nothing
           | otherwise = Just (G.init v, G.last v)
  filter = G.filter
  filterM = G.filterM
  replicate = G.replicate
  replicateM = G.replicateM
  -- groupBy : use default
  -- groupAllOn : use default
  -- subsequences : use default
  -- permutations : use default
  tailEx = G.tail
  -- tailMay : use default
  initEx = G.init
  -- initMay : use default
  unsafeTail = G.unsafeTail
  unsafeInit = G.unsafeInit
  index = (G.!?)
  indexEx = (G.!)
  unsafeIndex = G.unsafeIndex
  -- splitWhen : use default
  {-# INLINE fromList #-}
  {-# INLINE lengthIndex #-}
  {-# INLINE break #-}
  {-# INLINE span #-}
  {-# INLINE dropWhile #-}
  {-# INLINE takeWhile #-}
  {-# INLINE splitAt #-}
  {-# INLINE take #-}
  {-# INLINE unsafeTake #-}
  {-# INLINE drop #-}
  {-# INLINE unsafeDrop #-}
  {-# INLINE partition #-}
  {-# INLINE uncons #-}
  {-# INLINE unsnoc #-}
  {-# INLINE filter #-}
  {-# INLINE filterM #-}
  {-# INLINE replicate #-}
  {-# INLINE replicateM #-}
  {-# INLINE tailEx #-}
  {-# INLINE initEx #-}
  {-# INLINE unsafeTail #-}
  {-# INLINE unsafeInit #-}
  {-# INLINE index #-}
  {-# INLINE indexEx #-}
  {-# INLINE unsafeIndex #-}

-----

instance Unboxable Bool where
  type Rep Bool = Bool
  from = id
  to = id
  {-# INLINE from #-}
  {-# INLINE to #-}
instance Unboxable Char where
  type Rep Char = Char
  from = id
  to = id
  {-# INLINE from #-}
  {-# INLINE to #-}
instance Unboxable Double where
  type Rep Double = Double
  from = id
  to = id
  {-# INLINE from #-}
  {-# INLINE to #-}
instance Unboxable Float where
  type Rep Float = Float
  from = id
  to = id
  {-# INLINE from #-}
  {-# INLINE to #-}
instance Unboxable Int where
  type Rep Int = Int
  from = id
  to = id
  {-# INLINE from #-}
  {-# INLINE to #-}
instance Unboxable Int8 where
  type Rep Int8 = Int8
  from = id
  to = id
  {-# INLINE from #-}
  {-# INLINE to #-}
instance Unboxable Int16 where
  type Rep Int16 = Int16
  from = id
  to = id
  {-# INLINE from #-}
  {-# INLINE to #-}
instance Unboxable Int32 where
  type Rep Int32 = Int32
  from = id
  to = id
  {-# INLINE from #-}
  {-# INLINE to #-}
instance Unboxable Int64 where
  type Rep Int64 = Int64
  from = id
  to = id
  {-# INLINE from #-}
  {-# INLINE to #-}
instance Unboxable Word where
  type Rep Word = Word
  from = id
  to = id
  {-# INLINE from #-}
  {-# INLINE to #-}
instance Unboxable Word8 where
  type Rep Word8 = Word8
  from = id
  to = id
  {-# INLINE from #-}
  {-# INLINE to #-}
instance Unboxable Word16 where
  type Rep Word16 = Word16
  from = id
  to = id
  {-# INLINE from #-}
  {-# INLINE to #-}
instance Unboxable Word32 where
  type Rep Word32 = Word32
  from = id
  to = id
  {-# INLINE from #-}
  {-# INLINE to #-}
instance Unboxable Word64 where
  type Rep Word64 = Word64
  from = id
  to = id
  {-# INLINE from #-}
  {-# INLINE to #-}
instance Unboxable () where
  type Rep () = ()
  from = id
  to = id
  {-# INLINE from #-}
  {-# INLINE to #-}
instance Unboxable a => Unboxable (Data.Complex.Complex a) where
  type Rep (Data.Complex.Complex a) = Data.Complex.Complex (Rep a)
  from = fmap from
  to = fmap to
  {-# INLINE from #-}
  {-# INLINE to #-}
instance (Unboxable a, Unboxable b) => Unboxable (a, b) where
  type Rep (a, b) = (Rep a, Rep b)
  from (a, b) = (from a, from b) -- TODO: strictness?
  to (a, b) = (to a, to b)
  {-# INLINE from #-}
  {-# INLINE to #-}
instance (Unboxable a, Unboxable b, Unboxable c) => Unboxable (a, b, c) where
  type Rep (a, b, c) = (Rep a, Rep b, Rep c)
  from (a, b, c) = (from a, from b, from c)
  to (a, b, c) = (to a, to b, to c)
  {-# INLINE from #-}
  {-# INLINE to #-}
instance (Unboxable a, Unboxable b, Unboxable c, Unboxable d) => Unboxable (a, b, c, d) where
  type Rep (a, b, c, d) = (Rep a, Rep b, Rep c, Rep d)
  from (a, b, c, d) = (from a, from b, from c, from d)
  to (a, b, c, d) = (to a, to b, to c, to d)
  {-# INLINE from #-}
  {-# INLINE to #-}
instance (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e) => Unboxable (a, b, c, d, e) where
  type Rep (a, b, c, d, e) = (Rep a, Rep b, Rep c, Rep d, Rep e)
  from (a, b, c, d, e) = (from a, from b, from c, from d, from e)
  to (a, b, c, d, e) = (to a, to b, to c, to d, to e)
  {-# INLINE from #-}
  {-# INLINE to #-}
instance (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e, Unboxable f) => Unboxable (a, b, c, d, e, f) where
  type Rep (a, b, c, d, e, f) = (Rep a, Rep b, Rep c, Rep d, Rep e, Rep f)
  from (a, b, c, d, e, f) = (from a, from b, from c, from d, from e, from f)
  to (a, b, c, d, e, f) = (to a, to b, to c, to d, to e, to f)
  {-# INLINE from #-}
  {-# INLINE to #-}

-- Deriving 'Unboxable' instances of newtypes in base is possible via either GHC.Generics:
-- > instance Unboxable a => Unboxable (Data.Functor.Identity.Identity a)
-- or GeneralizedNewtypeDeriving + StandaloneDeriving:
-- > deriving instance Unboxable a => Unboxable (Data.Functor.Identity.Identity a)

deriving instance Unboxable a => Unboxable (Data.Functor.Identity.Identity a)
deriving instance Unboxable a => Unboxable (Data.Functor.Const.Const a b)
deriving instance Unboxable a => Unboxable (Data.Semigroup.Min a)
deriving instance Unboxable a => Unboxable (Data.Semigroup.Max a)
deriving instance Unboxable a => Unboxable (Data.Semigroup.First a)
deriving instance Unboxable a => Unboxable (Data.Semigroup.Last a)
deriving instance Unboxable a => Unboxable (Data.Semigroup.WrappedMonoid a)
deriving instance Unboxable a => Unboxable (Data.Monoid.Dual a)
deriving instance Unboxable Data.Monoid.All
deriving instance Unboxable Data.Monoid.Any
deriving instance Unboxable a => Unboxable (Data.Monoid.Sum a)
deriving instance Unboxable a => Unboxable (Data.Monoid.Product a)
deriving instance Unboxable a => Unboxable (Data.Ord.Down a)