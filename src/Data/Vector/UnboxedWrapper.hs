module Data.Vector.UnboxedWrapper
  (Unboxable(..)
  ,Vector
  ,module Data.Vector.Generic -- TODO
  ,coerceVector
  ,liftCoercion
  ,vectorCoercion
  ) where
import Data.Vector.Generic hiding (Vector)
import Data.Vector.UnboxedWrapper.Base
