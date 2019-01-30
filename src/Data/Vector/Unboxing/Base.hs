{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Vector.Unboxing.Base
  (Unboxable(Underlying) -- coercion is not exported
  ,Vector(UnboxingVector)
  ,MVector(UnboxingMVector)
  ,coerceVector
  ,liftCoercion
  ,vectorCoercion
  ) where
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Fusion.Bundle as Bundle
import Data.Coerce
import Data.Type.Coercion
import Data.Semigroup
import Data.Int
import Data.Word
import qualified Data.Complex
import qualified Data.Functor.Identity
import qualified Data.Functor.Const
import qualified Data.Ord
import qualified Data.Monoid
import GHC.Exts (IsList(..))
import Control.DeepSeq (NFData(..))
import Text.Read (Read(..),readListPrecDefault)

class (U.Unbox (Underlying a) {-, Coercible a (Underlying a) -}) => Unboxable a where
  -- | The underlying type of @a@.  Must be an instance of 'U.Unbox'.
  type Underlying a

  -- A hack to hide @Coercible a (Underlying a)@ from outside...
  coercion :: Coercion a (Underlying a)
  default coercion :: Coercible a (Underlying a) => Coercion a (Underlying a)
  coercion = Coercion
  {-# INLINE coercion #-}

-- This declaration is not possible:
-- type role Vector representational

newtype Vector a = UnboxingVector (U.Vector (Underlying a))
newtype MVector s a = UnboxingMVector (UM.MVector s (Underlying a))

type instance G.Mutable Vector = MVector

-- Coercible a b is not strictly necessary in this function, but the data constructors should be visible on the call site.
coerceVector :: (Coercible a b, Underlying a ~ Underlying b) => Vector a -> Vector b
coerceVector = coerce
{-# INLINE coerceVector #-}

liftCoercion :: (Underlying a ~ Underlying b) => Coercion a b -> Coercion (Vector a) (Vector b)
liftCoercion Coercion = Coercion
{-# INLINE liftCoercion #-}

vectorCoercion :: (Coercible a b, Underlying a ~ Underlying b) => Coercion (Vector a) (Vector b)
vectorCoercion = Coercion
{-# INLINE vectorCoercion #-}

-- This is not possible:
-- instance (Coercible a b, Underlying a ~ Underlying b) => Coercible (Vector a) (Vector b)

instance (Unboxable a) => IsList (Vector a) where
  type Item (Vector a) = a
  fromList xs         = case coercion @ a of Coercion -> UnboxingVector (fromList (coerce xs))
  fromListN n xs      = case coercion @ a of Coercion -> UnboxingVector (fromListN n (coerce xs))
  toList (UnboxingVector v) = case coercion @ a of Coercion -> coerce (toList v)
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
  basicUnsafeReplicate i x                                   = case coercion @ a of Coercion -> UnboxingMVector <$> GM.basicUnsafeReplicate i (coerce x)
  basicUnsafeRead (UnboxingMVector mv) i                     = case coercion @ a of Coercion -> coerce <$> GM.basicUnsafeRead mv i
  basicUnsafeWrite (UnboxingMVector mv) i x                  = case coercion @ a of Coercion -> GM.basicUnsafeWrite mv i (coerce x)
  basicClear (UnboxingMVector mv)                            = GM.basicClear mv
  basicSet (UnboxingMVector mv) x                            = case coercion @ a of Coercion -> GM.basicSet mv (coerce x)
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
  basicUnsafeIndexM (UnboxingVector v) i                  = case coercion @ a of Coercion -> coerce <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (UnboxingMVector mv) (UnboxingVector v) = G.basicUnsafeCopy mv v
  elemseq (UnboxingVector v) x y                          = case coercion @ a of Coercion -> G.elemseq v (coerce x) y
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE elemseq #-}

-----

-- Unboxable instances

instance Unboxable Bool where   type Underlying Bool = Bool
instance Unboxable Char where   type Underlying Char = Char
instance Unboxable Double where type Underlying Double = Double
instance Unboxable Float where  type Underlying Float = Float
instance Unboxable Int where    type Underlying Int = Int
instance Unboxable Int8 where   type Underlying Int8 = Int8
instance Unboxable Int16 where  type Underlying Int16 = Int16
instance Unboxable Int32 where  type Underlying Int32 = Int32
instance Unboxable Int64 where  type Underlying Int64 = Int64
instance Unboxable Word where   type Underlying Word = Word
instance Unboxable Word8 where  type Underlying Word8 = Word8
instance Unboxable Word16 where type Underlying Word16 = Word16
instance Unboxable Word32 where type Underlying Word32 = Word32
instance Unboxable Word64 where type Underlying Word64 = Word64
instance Unboxable () where     type Underlying () = ()

instance (Unboxable a) => Unboxable (Data.Complex.Complex a) where
  type Underlying (Data.Complex.Complex a) = Data.Complex.Complex (Underlying a)
  coercion = case coercion @ a of Coercion -> Coercion
  {-# INLINE coercion #-}

instance (Unboxable a, Unboxable b) => Unboxable (a, b) where
  type Underlying (a, b) = (Underlying a, Underlying b)
  coercion = case coercion @ a of
    Coercion -> case coercion @ b of
      Coercion -> Coercion
  {-# INLINE coercion #-}

instance (Unboxable a, Unboxable b, Unboxable c) => Unboxable (a, b, c) where
  type Underlying (a, b, c) = (Underlying a, Underlying b, Underlying c)
  coercion = case coercion @ a of
    Coercion -> case coercion @ b of
      Coercion -> case coercion @ c of
        Coercion -> Coercion
  {-# INLINE coercion #-}

instance (Unboxable a, Unboxable b, Unboxable c, Unboxable d) => Unboxable (a, b, c, d) where
  type Underlying (a, b, c, d) = (Underlying a, Underlying b, Underlying c, Underlying d)
  coercion = case coercion @ a of
    Coercion -> case coercion @ b of
      Coercion -> case coercion @ c of
        Coercion -> case coercion @ d of
          Coercion -> Coercion
  {-# INLINE coercion #-}

instance (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e) => Unboxable (a, b, c, d, e) where
  type Underlying (a, b, c, d, e) = (Underlying a, Underlying b, Underlying c, Underlying d, Underlying e)
  coercion = case coercion @ a of
    Coercion -> case coercion @ b of
      Coercion -> case coercion @ c of
        Coercion -> case coercion @ d of
          Coercion -> case coercion @ e of
            Coercion -> Coercion
  {-# INLINE coercion #-}

instance (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e, Unboxable f) => Unboxable (a, b, c, d, e, f) where
  type Underlying (a, b, c, d, e, f) = (Underlying a, Underlying b, Underlying c, Underlying d, Underlying e, Underlying f)
  coercion = case coercion @ a of
    Coercion -> case coercion @ b of
      Coercion -> case coercion @ c of
        Coercion -> case coercion @ d of
          Coercion -> case coercion @ e of
            Coercion -> case coercion @ f of
              Coercion -> Coercion
  {-# INLINE coercion #-}

instance (Unboxable a) => Unboxable (Data.Functor.Identity.Identity a) where
  type Underlying (Data.Functor.Identity.Identity a) = Underlying a
  coercion = case coercion @ a of Coercion -> Coercion
  {-# INLINE coercion #-}

instance (Unboxable a) => Unboxable (Data.Functor.Const.Const a b) where
  type Underlying (Data.Functor.Const.Const a b) = Underlying a
  coercion = case coercion @ a of Coercion -> Coercion
  {-# INLINE coercion #-}

instance (Unboxable a) => Unboxable (Data.Semigroup.Min a) where
  type Underlying (Data.Semigroup.Min a) = Underlying a
  coercion = case coercion @ a of Coercion -> Coercion
  {-# INLINE coercion #-}

instance (Unboxable a) => Unboxable (Data.Semigroup.Max a) where
  type Underlying (Data.Semigroup.Max a) = Underlying a
  coercion = case coercion @ a of Coercion -> Coercion
  {-# INLINE coercion #-}

instance (Unboxable a) => Unboxable (Data.Semigroup.First a) where
  type Underlying (Data.Semigroup.First a) = Underlying a
  coercion = case coercion @ a of Coercion -> Coercion
  {-# INLINE coercion #-}

instance (Unboxable a) => Unboxable (Data.Semigroup.Last a) where
  type Underlying (Data.Semigroup.Last a) = Underlying a
  coercion = case coercion @ a of Coercion -> Coercion
  {-# INLINE coercion #-}

instance (Unboxable a) => Unboxable (Data.Semigroup.WrappedMonoid a) where
  type Underlying (Data.Semigroup.WrappedMonoid a) = Underlying a
  coercion = case coercion @ a of Coercion -> Coercion
  {-# INLINE coercion #-}

instance (Unboxable a) => Unboxable (Data.Monoid.Dual a) where
  type Underlying (Data.Monoid.Dual a) = Underlying a
  coercion = case coercion @ a of Coercion -> Coercion
  {-# INLINE coercion #-}

instance Unboxable Data.Monoid.All where
  type Underlying Data.Monoid.All = Bool

instance Unboxable Data.Monoid.Any where
  type Underlying Data.Monoid.Any = Bool

instance (Unboxable a) => Unboxable (Data.Monoid.Sum a) where
  type Underlying (Data.Monoid.Sum a) = Underlying a
  coercion = case coercion @ a of Coercion -> Coercion
  {-# INLINE coercion #-}

instance (Unboxable a) => Unboxable (Data.Monoid.Product a) where
  type Underlying (Data.Monoid.Product a) = Underlying a
  coercion = case coercion @ a of Coercion -> Coercion
  {-# INLINE coercion #-}

instance (Unboxable a) => Unboxable (Data.Ord.Down a) where
  type Underlying (Data.Ord.Down a) = Underlying a
  coercion = case coercion @ a of Coercion -> Coercion
  {-# INLINE coercion #-}
