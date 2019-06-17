{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
module Enum where
import qualified Data.Vector.Unboxing as V

data OrderingEq = LT | LE | EQ | GE | GT
  deriving (Eq, Ord, Enum, Bounded, Read, Show)
  deriving V.Unboxable via V.Enum OrderingEq
