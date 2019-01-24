module Data.Vector.UnboxedWrapper
  (Unboxable(..)
  ,Vector
  ,module Data.Vector.Generic -- TODO
  ,coerceVector
  ) where
import Data.Vector.Generic hiding (Vector)
import Data.Vector.UnboxedWrapper.Base
