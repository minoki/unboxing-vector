{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Vector.Unboxing.Internal
  (Unboxable(Rep)
  ,Vector(UnboxingVector)
  ,MVector(UnboxingMVector)
  ,Generics(..)
  ,coerceVector
  ,liftCoercion
  ,vectorCoercion
  ,toUnboxedVector
  ,fromUnboxedVector
  ,coercionWithUnboxedVector
  ,toUnboxedMVector
  ,fromUnboxedMVector
  ,coercionWithUnboxedMVector
  ) where
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Fusion.Bundle as Bundle
import Data.Coerce
import Data.Type.Coercion
import Data.Int
import Data.Word
import qualified GHC.Generics
import Data.Type.Bool
import qualified Data.Complex
import qualified Data.Functor.Identity
import qualified Data.Functor.Const
import qualified Data.Ord
import qualified Data.Semigroup
import qualified Data.Monoid
import qualified Data.MonoTraversable -- from mono-traversable
import qualified Data.Sequences       -- from mono-traversable
import GHC.Exts (IsList(..))
import Control.DeepSeq (NFData(..))
import Text.Read (Read(..),readListPrecDefault)
import GHC.TypeLits (TypeError,ErrorMessage(Text))

newtype Vector a = UnboxingVector (U.Vector (Rep a))
newtype MVector s a = UnboxingMVector (UM.MVector s (Rep a))

type instance G.Mutable Vector = MVector

-- | Types that can be stored in unboxed vectors ('Vector' and 'MVector').
--
-- This class can be derived with @GeneralizedNewtypeDeriving@
-- (you may need @UndecidableInstances@ in addition).
class U.Unbox (Rep a) => Unboxable a where
  -- | The underlying type of @a@.  Must be an instance of 'U.Unbox'.
  type Rep a

  -- Hidden members:

  type CoercibleRep a
  type CoercibleRep a = Rep a

  -- True if both 'unboxingFrom and 'unboxingTo' are just 'coerce'
  type IsTrivial a :: Bool
  type IsTrivial a = 'True
  -- TODO: Use ConstraintKinds?

  unboxingFrom :: a -> Rep a
  default unboxingFrom :: Coercible a (Rep a) => a -> Rep a
  unboxingFrom = coerce
  {-# INLINE unboxingFrom #-}

  unboxingTo :: Rep a -> a
  default unboxingTo :: Coercible a (Rep a) => Rep a -> a
  unboxingTo = coerce
  {-# INLINE unboxingTo #-}

coerceVector :: (Coercible a b, Unboxable a, Unboxable b, CoercibleRep a ~ CoercibleRep b, Rep a ~ Rep b) => Vector a -> Vector b
coerceVector = coerce
{-# INLINE coerceVector #-}

liftCoercion :: (Unboxable a, Unboxable b, CoercibleRep a ~ CoercibleRep b, Rep a ~ Rep b) => Coercion a b -> Coercion (Vector a) (Vector b)
liftCoercion Coercion = Coercion
{-# INLINE liftCoercion #-}

vectorCoercion :: (Coercible a b, Unboxable a, Unboxable b, CoercibleRep a ~ CoercibleRep b, Rep a ~ Rep b) => Coercion (Vector a) (Vector b)
vectorCoercion = Coercion
{-# INLINE vectorCoercion #-}

toUnboxedVector :: (Unboxable a, Rep a ~ a, IsTrivial a ~ 'True) => Vector a -> U.Vector a
toUnboxedVector = coerce
{-# INLINE toUnboxedVector #-}

fromUnboxedVector :: (Unboxable a, Rep a ~ a, IsTrivial a ~ 'True) => U.Vector a -> Vector a
fromUnboxedVector = coerce
{-# INLINE fromUnboxedVector #-}

toUnboxedMVector :: (Unboxable a, Rep a ~ a, IsTrivial a ~ 'True) => MVector s a -> UM.MVector s a
toUnboxedMVector = coerce
{-# INLINE toUnboxedMVector #-}

fromUnboxedMVector :: (Unboxable a, Rep a ~ a, IsTrivial a ~ 'True) => UM.MVector s a -> MVector s a
fromUnboxedMVector = coerce
{-# INLINE fromUnboxedMVector #-}

coercionWithUnboxedVector :: (Unboxable a, Rep a ~ a, IsTrivial a ~ 'True) => Coercion (Vector a) (U.Vector a)
coercionWithUnboxedVector = Coercion
{-# INLINE coercionWithUnboxedVector #-}

coercionWithUnboxedMVector :: (Unboxable a, Rep a ~ a, IsTrivial a ~ 'True) => Coercion (MVector s a) (U.MVector s a)
coercionWithUnboxedMVector = Coercion
{-# INLINE coercionWithUnboxedMVector #-}

-----

-- Generics

-- | A newtype wrapper for use with @DerivingVia@.
newtype Generics a = Generics a

instance (GHC.Generics.Generic a, Unboxable (Rep' (GHC.Generics.Rep a)), Unboxable' (GHC.Generics.Rep a)) => Unboxable (Generics a) where
  type Rep (Generics a) = Rep (Rep' (GHC.Generics.Rep a))
  type CoercibleRep (Generics a) = a
  type IsTrivial (Generics a) = 'False
  unboxingFrom (Generics x) = unboxingFrom (from' (GHC.Generics.from x))
  {-# INLINE unboxingFrom #-}
  unboxingTo y = Generics (GHC.Generics.to (to' (unboxingTo y)))
  {-# INLINE unboxingTo #-}

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
  from' = {- from . GHC.Generics.unK1 -} coerce (unboxingFrom :: c -> Rep c)
  to' = {- GHC.Generics.K1 . to -} coerce (unboxingTo :: Rep c -> c)
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

-----

-- Instances

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
  basicUnsafeReplicate i x                                   = UnboxingMVector <$> GM.basicUnsafeReplicate i (unboxingFrom x)
  basicUnsafeRead (UnboxingMVector mv) i                     = unboxingTo <$> GM.basicUnsafeRead mv i
  basicUnsafeWrite (UnboxingMVector mv) i x                  = GM.basicUnsafeWrite mv i (unboxingFrom x)
  basicClear (UnboxingMVector mv)                            = GM.basicClear mv
  basicSet (UnboxingMVector mv) x                            = GM.basicSet mv (unboxingFrom x)
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
  basicUnsafeIndexM (UnboxingVector v) i                  = unboxingTo <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (UnboxingMVector mv) (UnboxingVector v) = G.basicUnsafeCopy mv v
  elemseq (UnboxingVector v) x y                          = G.elemseq v (unboxingFrom x) y -- ?
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

-- Unboxable instances

instance Unboxable Bool where   type Rep Bool = Bool
instance Unboxable Char where   type Rep Char = Char
instance Unboxable Double where type Rep Double = Double
instance Unboxable Float where  type Rep Float = Float
instance Unboxable Int where    type Rep Int = Int
instance Unboxable Int8 where   type Rep Int8 = Int8
instance Unboxable Int16 where  type Rep Int16 = Int16
instance Unboxable Int32 where  type Rep Int32 = Int32
instance Unboxable Int64 where  type Rep Int64 = Int64
instance Unboxable Word where   type Rep Word = Word
instance Unboxable Word8 where  type Rep Word8 = Word8
instance Unboxable Word16 where type Rep Word16 = Word16
instance Unboxable Word32 where type Rep Word32 = Word32
instance Unboxable Word64 where type Rep Word64 = Word64
instance Unboxable () where     type Rep () = ()

instance (Unboxable a) => Unboxable (Data.Complex.Complex a) where
  type Rep (Data.Complex.Complex a) = Data.Complex.Complex (Rep a)
  type CoercibleRep (Data.Complex.Complex a) = Data.Complex.Complex (CoercibleRep a)
  type IsTrivial (Data.Complex.Complex a) = IsTrivial a
  unboxingFrom = fmap unboxingFrom
  unboxingTo = fmap unboxingTo
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance (Unboxable a, Unboxable b) => Unboxable (a, b) where
  type Rep (a, b) = (Rep a, Rep b)
  type CoercibleRep (a, b) = (CoercibleRep a, CoercibleRep b)
  type IsTrivial (a, b) = IsTrivial a && IsTrivial b
  unboxingFrom (a, b) = (unboxingFrom a, unboxingFrom b)
  unboxingTo (a, b) = (unboxingTo a, unboxingTo b)
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance (Unboxable a, Unboxable b, Unboxable c) => Unboxable (a, b, c) where
  type Rep (a, b, c) = (Rep a, Rep b, Rep c)
  type CoercibleRep (a, b, c) = (CoercibleRep a, CoercibleRep b, CoercibleRep c)
  type IsTrivial (a, b, c) = IsTrivial a && IsTrivial b && IsTrivial c
  unboxingFrom (a, b, c) = (unboxingFrom a, unboxingFrom b, unboxingFrom c)
  unboxingTo (a, b, c) = (unboxingTo a, unboxingTo b, unboxingTo c)
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance (Unboxable a, Unboxable b, Unboxable c, Unboxable d) => Unboxable (a, b, c, d) where
  type Rep (a, b, c, d) = (Rep a, Rep b, Rep c, Rep d)
  type CoercibleRep (a, b, c, d) = (CoercibleRep a, CoercibleRep b, CoercibleRep c, CoercibleRep d)
  type IsTrivial (a, b, c, d) = IsTrivial a && IsTrivial b && IsTrivial c && IsTrivial d
  unboxingFrom (a, b, c, d) = (unboxingFrom a, unboxingFrom b, unboxingFrom c, unboxingFrom d)
  unboxingTo (a, b, c, d) = (unboxingTo a, unboxingTo b, unboxingTo c, unboxingTo d)
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e) => Unboxable (a, b, c, d, e) where
  type Rep (a, b, c, d, e) = (Rep a, Rep b, Rep c, Rep d, Rep e)
  type CoercibleRep (a, b, c, d, e) = (CoercibleRep a, CoercibleRep b, CoercibleRep c, CoercibleRep d, CoercibleRep e)
  type IsTrivial (a, b, c, d, e) = IsTrivial a && IsTrivial b && IsTrivial c && IsTrivial d && IsTrivial e
  unboxingFrom (a, b, c, d, e) = (unboxingFrom a, unboxingFrom b, unboxingFrom c, unboxingFrom d, unboxingFrom e)
  unboxingTo (a, b, c, d, e) = (unboxingTo a, unboxingTo b, unboxingTo c, unboxingTo d, unboxingTo e)
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e, Unboxable f) => Unboxable (a, b, c, d, e, f) where
  type Rep (a, b, c, d, e, f) = (Rep a, Rep b, Rep c, Rep d, Rep e, Rep f)
  type CoercibleRep (a, b, c, d, e, f) = (CoercibleRep a, CoercibleRep b, CoercibleRep c, CoercibleRep d, CoercibleRep e, CoercibleRep f)
  type IsTrivial (a, b, c, d, e, f) = IsTrivial a && IsTrivial b && IsTrivial c && IsTrivial d && IsTrivial e && IsTrivial f
  unboxingFrom (a, b, c, d, e, f) = (unboxingFrom a, unboxingFrom b, unboxingFrom c, unboxingFrom d, unboxingFrom e, unboxingFrom f)
  unboxingTo (a, b, c, d, e, f) = (unboxingTo a, unboxingTo b, unboxingTo c, unboxingTo d, unboxingTo e, unboxingTo f)
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

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
