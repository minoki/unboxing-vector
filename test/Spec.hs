{-# LANGUAGE TypeFamilies #-}
import Prelude
import Test.HUnit
import TestTypeErrors
import qualified Data.Vector.Unboxing as V
import Data.Monoid (Sum(..))
import Foo (Foo,mkFoo)
import Data.MonoTraversable (ofold)

newtype IntMod17 = IntMod17 Int deriving (Eq,Show)

instance V.Unboxable IntMod17 where
  type Underlying IntMod17 = Int

instance Num IntMod17 where
  IntMod17 x + IntMod17 y = IntMod17 ((x + y) `rem` 17)
  IntMod17 x - IntMod17 y = IntMod17 ((x - y) `mod` 17)
  IntMod17 x * IntMod17 y = IntMod17 ((x * y) `rem` 17)
  negate (IntMod17 x) = IntMod17 (negate x `mod` 17)
  fromInteger x = IntMod17 (fromIntegral (x `mod` 17))
  abs = undefined; signum = undefined

testIntMod17 = TestCase $ do
  let v = V.fromList [-3,-2,-1,0,1,2,3,4,5] :: V.Vector IntMod17
  assertEqual "construction" (V.fromList [14,15,16,0,1,2,3,4,5]) v
  assertEqual "sum" 9 (V.sum v) -- not 60
  assertEqual "coercion" (V.fromList [14,15,16,0,1,2,3,4,5] :: V.Vector Int) (V.coerceVector v) -- this is possible because the constructor of IntMod17 is visible here
  let vSum = V.coerceVector v :: V.Vector (Sum IntMod17)
  assertEqual "coercion and sum" (Sum 9) (ofold vSum)

-- We can make an unboxed vector of Foo, even though we don't have 'Coercible Int Foo' in scope.
testAbstractType = TestCase $ assertEqual "Foo" (V.head (V.singleton mkFoo :: V.Vector Foo)) mkFoo

tests = TestList [TestLabel "Basic features" testIntMod17
                 ,TestLabel "Test with abstract type" testAbstractType
                 ,TestLabel "Check for type errors" testTypeErrors
                 ]

main = runTestTT tests
