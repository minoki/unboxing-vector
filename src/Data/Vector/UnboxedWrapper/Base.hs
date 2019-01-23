{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Vector.UnboxedWrapper.Base where
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Fusion.Bundle as Bundle
import Data.Coerce
import Data.Semigroup

class (U.Unbox (Underlying a), Coercible a (Underlying a)) => Wrapper a where
  type Underlying a

newtype Vector a = UWVector (U.Vector (Underlying a))
newtype MVector s a = UWMVector (UM.MVector s (Underlying a))

type instance G.Mutable Vector = MVector

-- IsList

instance (Eq a, Wrapper a) => Eq (Vector a) where
  xs == ys = Bundle.eq (G.stream xs) (G.stream ys)
  xs /= ys = not (Bundle.eq (G.stream xs) (G.stream ys))

instance (Show a, Wrapper a) => Show (Vector a) where
  showsPrec = G.showsPrec

instance (Wrapper a) => Semigroup (Vector a) where
  (<>) = (G.++)
  sconcat = G.concatNE

instance (Wrapper a) => Monoid (Vector a) where
  mempty = G.empty
  mappend = (<>)
  mconcat = G.concat

-- Data, Ord, Read, NFData

instance (Wrapper a) => GM.MVector MVector a where
  basicLength (UWMVector mv) = GM.basicLength mv
  basicUnsafeSlice i l (UWMVector mv) = UWMVector (GM.basicUnsafeSlice i l mv)
  basicOverlaps (UWMVector mv) (UWMVector mv') = GM.basicOverlaps mv mv'
  basicUnsafeNew l = UWMVector <$> GM.basicUnsafeNew l
  basicInitialize (UWMVector mv) = GM.basicInitialize mv
  basicUnsafeReplicate i x = UWMVector <$> GM.basicUnsafeReplicate i (coerce x)
  basicUnsafeRead (UWMVector mv) i = coerce <$> GM.basicUnsafeRead mv i
  basicUnsafeWrite (UWMVector mv) i x = GM.basicUnsafeWrite mv i (coerce x)
  basicClear (UWMVector mv) = GM.basicClear mv
  basicSet (UWMVector mv) x = GM.basicSet mv (coerce x)
  basicUnsafeCopy (UWMVector mv) (UWMVector mv') = GM.basicUnsafeCopy mv mv'
  basicUnsafeMove (UWMVector mv) (UWMVector mv') = GM.basicUnsafeMove mv mv'
  basicUnsafeGrow (UWMVector mv) n = UWMVector <$> GM.basicUnsafeGrow mv n

instance (Wrapper a) => G.Vector Vector a where
  basicUnsafeFreeze (UWMVector mv) = UWVector <$> G.basicUnsafeFreeze mv
  basicUnsafeThaw (UWVector v) = UWMVector <$> G.basicUnsafeThaw v
  basicLength (UWVector v) = G.basicLength v
  basicUnsafeSlice i l (UWVector v) = UWVector (G.basicUnsafeSlice i l v)
  basicUnsafeIndexM (UWVector v) i = coerce <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (UWMVector mv) (UWVector v) = G.basicUnsafeCopy mv v
  elemseq (UWVector v) x y = G.elemseq v (coerce x) y
