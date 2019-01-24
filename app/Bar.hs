{-# LANGUAGE TypeFamilies #-}
module Bar (Bar,mkBarVec,Baz,mkBaz) where
import Data.Int
import qualified Data.Vector.UnboxedWrapper as V

newtype Bar = Bar Int64 deriving (Show)
instance V.Unboxable Bar where
  type Underlying Bar = Int64

mkBarVec :: V.Vector Bar
mkBarVec = V.generate 10 (\i -> Bar (fromIntegral i))

newtype Baz = Baz Int64 deriving (Show)

mkBaz :: Int64 -> Baz
mkBaz = Baz
