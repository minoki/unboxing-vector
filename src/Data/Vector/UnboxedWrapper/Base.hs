{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Vector.UnboxedWrapper.Base
  (Unboxable(Underlying) -- coercion is not exported
  ,Vector(UWVector)
  ,MVector(UWMVector)
  ,coerceVector
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
import Data.Complex
import GHC.Exts (IsList(..))
import Data.Data (Data(..))
import Control.DeepSeq (NFData(..))
import Text.Read (Read(..),readListPrecDefault)

class (U.Unbox (Underlying a) {-, Coercible a (Underlying a) -}) => Unboxable a where
  type Underlying a

  -- A dirty hack to hide @Coercible a (Underlying a)@ from any class...
  coercion :: Coercion a (Underlying a)
  default coercion :: Coercible a (Underlying a) => Coercion a (Underlying a)
  coercion = Coercion
  {-# INLINE coercion #-}

instance Unboxable Bool where
  type Underlying Bool = Bool
instance Unboxable Char where
  type Underlying Char = Char
instance Unboxable Double where
  type Underlying Double = Double
instance Unboxable Float where
  type Underlying Float = Float
instance Unboxable Int where
  type Underlying Int = Int
instance Unboxable Int8 where
  type Underlying Int8 = Int8
instance Unboxable Int16 where
  type Underlying Int16 = Int16
instance Unboxable Int32 where
  type Underlying Int32 = Int32
instance Unboxable Int64 where
  type Underlying Int64 = Int64
instance Unboxable Word where
  type Underlying Word = Word
instance Unboxable Word8 where
  type Underlying Word8 = Word8
instance Unboxable Word16 where
  type Underlying Word16 = Word16
instance Unboxable Word32 where
  type Underlying Word32 = Word32
instance Unboxable Word64 where
  type Underlying Word64 = Word64
instance Unboxable () where
  type Underlying () = ()
instance (Unboxable a) => Unboxable (Complex a) where
  type Underlying (Complex a) = Complex (Underlying a)
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

newtype Vector a = UWVector (U.Vector (Underlying a))
newtype MVector s a = UWMVector (UM.MVector s (Underlying a))

type instance G.Mutable Vector = MVector

-- Coercible a b is not strictly necessary in this function, but the data constructors should be visible on the call site.
coerceVector :: (Coercible a b, Underlying a ~ Underlying b) => Vector a -> Vector b
coerceVector = coerce
{-# INLINE coerceVector #-}

instance (Unboxable a) => IsList (Vector a) where
  type Item (Vector a) = a
  fromList xs         = case coercion @ a of Coercion -> UWVector (fromList (coerce xs))
  fromListN n xs      = case coercion @ a of Coercion -> UWVector (fromListN n (coerce xs))
  toList (UWVector v) = case coercion @ a of Coercion -> coerce (toList v)

instance (Eq a, Unboxable a) => Eq (Vector a) where
  xs == ys = Bundle.eq (G.stream xs) (G.stream ys)
  xs /= ys = not (Bundle.eq (G.stream xs) (G.stream ys))

instance (Ord a, Unboxable a) => Ord (Vector a) where
  compare xs ys = Bundle.cmp (G.stream xs) (G.stream ys)

instance (Show a, Unboxable a) => Show (Vector a) where
  showsPrec = G.showsPrec

instance (Read a, Unboxable a) => Read (Vector a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault

instance (Unboxable a) => Semigroup (Vector a) where
  (<>) = (G.++)
  sconcat = G.concatNE

instance (Unboxable a) => Monoid (Vector a) where
  mempty = G.empty
  mappend = (<>)
  mconcat = G.concat

instance NFData (Vector a) where
  rnf !_ = () -- the content is unboxed

-- Is it okay with this?
instance (Data a, Unboxable a) => Data (Vector a) where
  gfoldl       = G.gfoldl
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = G.mkType "Data.Vector.UnboxedUnboxable.Vector"
  dataCast1    = G.dataCast

instance (Unboxable a) => GM.MVector MVector a where
  basicLength (UWMVector mv)                     = GM.basicLength mv
  basicUnsafeSlice i l (UWMVector mv)            = UWMVector (GM.basicUnsafeSlice i l mv)
  basicOverlaps (UWMVector mv) (UWMVector mv')   = GM.basicOverlaps mv mv'
  basicUnsafeNew l                               = UWMVector <$> GM.basicUnsafeNew l
  basicInitialize (UWMVector mv)                 = GM.basicInitialize mv
  basicUnsafeReplicate i x                       = case coercion @ a of Coercion -> UWMVector <$> GM.basicUnsafeReplicate i (coerce x)
  basicUnsafeRead (UWMVector mv) i               = case coercion @ a of Coercion -> coerce <$> GM.basicUnsafeRead mv i
  basicUnsafeWrite (UWMVector mv) i x            = case coercion @ a of Coercion -> GM.basicUnsafeWrite mv i (coerce x)
  basicClear (UWMVector mv)                      = GM.basicClear mv
  basicSet (UWMVector mv) x                      = case coercion @ a of Coercion -> GM.basicSet mv (coerce x)
  basicUnsafeCopy (UWMVector mv) (UWMVector mv') = GM.basicUnsafeCopy mv mv'
  basicUnsafeMove (UWMVector mv) (UWMVector mv') = GM.basicUnsafeMove mv mv'
  basicUnsafeGrow (UWMVector mv) n               = UWMVector <$> GM.basicUnsafeGrow mv n

instance (Unboxable a) => G.Vector Vector a where
  basicUnsafeFreeze (UWMVector mv)            = UWVector <$> G.basicUnsafeFreeze mv
  basicUnsafeThaw (UWVector v)                = UWMVector <$> G.basicUnsafeThaw v
  basicLength (UWVector v)                    = G.basicLength v
  basicUnsafeSlice i l (UWVector v)           = UWVector (G.basicUnsafeSlice i l v)
  basicUnsafeIndexM (UWVector v) i            = case coercion @ a of Coercion ->  coerce <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (UWMVector mv) (UWVector v) = G.basicUnsafeCopy mv v
  elemseq (UWVector v) x y                    = case coercion @ a of Coercion -> G.elemseq v (coerce x) y
