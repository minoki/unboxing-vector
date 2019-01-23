{-# LANGUAGE TypeFamilies #-}
module Main where
import qualified Data.Vector.UnboxedWrapper as V
import Data.Int

newtype Foo = Foo Int64 deriving (Show)
instance V.Wrapper Foo where
  type Underlying Foo = Int64

main :: IO ()
main = do
  print (V.generate 10 (\i -> Foo (fromIntegral i)) :: V.Vector Foo)
