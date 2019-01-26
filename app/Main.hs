{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Main where
import qualified Data.Vector.UnboxedWrapper as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Int
import Data.Coerce
import Data.Type.Coercion
import Data.Ord
import Bar

newtype Foo = Foo Int64
  deriving (Show {-, U.Unbox, GM.MVector UM.MVector, G.Vector U.Vector-})
  -- deriving anyclass V.Unboxable
instance V.Unboxable Foo where
  type Underlying Foo = Int64
--type instance V.Underlying Foo = Int64

--instance V.Unboxable Baz where
  --type Underlying Baz = Int64
  -- coercion = undefined

{-
newtype instance UM.MVector s Foo = MV_Foo (UM.MVector s Int64)
newtype instance U.Vector Foo = V_Foo (U.Vector Int64)

instance GM.MVector UM.MVector Foo where
  basicLength (MV_Foo mv) = GM.basicLength mv
  basicUnsafeSlice i l (MV_Foo mv) = MV_Foo (GM.basicUnsafeSlice i l mv)
  basicOverlaps (MV_Foo mv) (MV_Foo mv') = GM.basicOverlaps mv mv'
  basicUnsafeNew l = MV_Foo <$> GM.basicUnsafeNew l
  basicInitialize (MV_Foo mv) = GM.basicInitialize mv
  basicUnsafeReplicate i x = MV_Foo <$> GM.basicUnsafeReplicate i (coerce x)
  basicUnsafeRead (MV_Foo mv) i = coerce <$> GM.basicUnsafeRead mv i
  basicUnsafeWrite (MV_Foo mv) i x = GM.basicUnsafeWrite mv i (coerce x)
  basicClear (MV_Foo mv) = GM.basicClear mv
  basicSet (MV_Foo mv) x = GM.basicSet mv (coerce x)
  basicUnsafeCopy (MV_Foo mv) (MV_Foo mv') = GM.basicUnsafeCopy mv mv'
  basicUnsafeMove (MV_Foo mv) (MV_Foo mv') = GM.basicUnsafeMove mv mv'
  basicUnsafeGrow (MV_Foo mv) n = MV_Foo <$> GM.basicUnsafeGrow mv n

instance G.Vector U.Vector Foo where
  basicUnsafeFreeze (MV_Foo mv) = V_Foo <$> G.basicUnsafeFreeze mv
  basicUnsafeThaw (V_Foo v) = MV_Foo <$> G.basicUnsafeThaw v
  basicLength (V_Foo v) = G.basicLength v
  basicUnsafeSlice i l (V_Foo v) = V_Foo (G.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_Foo v) i = coerce <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Foo mv) (V_Foo v) = G.basicUnsafeCopy mv v
  elemseq (V_Foo v) x y = G.elemseq v (coerce x) y

instance U.Unbox Foo
-}

{-
getInt64Val :: (V.Unboxable a, V.Underlying a ~ Int64, a ~ Bar) => a -> Int64
getInt64Val = coerce
-}

main :: IO ()
main = do
  print (V.generate 10 (\i -> Foo (fromIntegral i)) :: V.Vector Foo)
  print (V.coerceVector (V.generate 10 (\i -> Foo (fromIntegral i)) :: V.Vector Foo) :: V.Vector Int64)
  print ((case V.liftCoercion (Coercion :: Coercion Foo Int64) of
           Coercion -> coerce [V.generate 10 (\i -> Foo (fromIntegral i)) :: V.Vector Foo]) :: [V.Vector Int64])
  print ((case V.vectorCoercion :: Coercion (V.Vector Foo) (V.Vector Int64) of
           Coercion -> coerce (Just (V.generate 10 (\i -> Foo (fromIntegral i)) :: V.Vector Foo))) :: Maybe (V.Vector Int64))
  -- print (U.generate 10 (\i -> Foo (fromIntegral i)) :: U.Vector Foo)
  -- print (coerce (U.generate 10 (\i -> Foo (fromIntegral i)) :: U.Vector Foo) :: U.Vector Int64)
  print mkBarVec
  -- print (V.coerceVector mkBarVec :: V.Vector Int64) -- should be invalid!
  -- print (getInt64Val $ V.head mkBarVec)
  -- print (coerce (V.head mkBarVec) :: Int64)
  -- print (V.singleton (mkBaz 42))
  let v = V.fromList [1,2,7] :: V.Vector Int
  let w = V.fromList [2,-3,7] :: V.Vector Int
  print (compare v w)
  print (compare (V.coerceVector v :: V.Vector (Down Int)) (V.coerceVector w))
  let v = U.fromList [1,2,7] :: U.Vector Int
  let w = U.fromList [2,-3,7] :: U.Vector Int
  print (compare v w)
  -- print (compare (coerce v :: U.Vector (Down Int)) (coerce w))
