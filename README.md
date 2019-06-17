# unboxing-vector

This package provides newtype-friendly wrappers for `Data.Vector.Unboxed` in [`vector` package](http://hackage.haskell.org/package/vector).

## Description

Suppose you define a newtype for `Int` and want to store them in an unboxed vector.

```haskell
newtype Foo = Foo Int

generate 10 (\i -> Foo i) :: Data.Vector.Unboxed.Vector Foo
```

With plain `Data.Vector.Unboxed`, you either write two dozen of lines of code to get it work (the exact code is [here](test/Foo.hs)), or resort to Template Haskell ([`vector-th-unbox` package](http://hackage.haskell.org/package/vector-th-unbox)) to generate it.

But with `Data.Vector.Unboxing`, the code you write is just two lines:

```haskell
instance Data.Vector.Unboxing.Unboxable Foo where
  type Rep Foo = Int

generate 10 (\i -> Foo i) :: Data.Vector.Unboxing.Vector Foo
```

...and if you want to be even more concise, you can derive `Unboxable` instance with `GeneralizedNewtypeDeriving`.

Note that the vector type provided by this package (`Data.Vector.Unboxing.Vector`) is *different* from `Data.Vector.Unboxed.Vector`.

The module defining the type `Foo` does not need to export its constructor to enable use of `Vector Foo`.

## For non-newtypes

Suppose you define a datatype isomorphic to a tuple of primitive types, like:

```haskell
data ComplexDouble = MkComplexDouble {-# UNPACK #-} !Double {-# UNPACK #-} !Double
```

In this example, `ComplexDouble` is isomorphic to `(Double, Double)`, but has a different representation. Thus, you cannot derive `Data.Vector.Unboxing.Unboxable` from `(Double, Double)`.

For such cases, unboxing-vector provides a feature to derive `Unboxable` using `Generic`.

```haskell
{-# LANGUAGE DeriveGeneric, DerivingVia, UndecidableInstances #-}

data ComplexDouble = ..
  deriving Generic
  deriving Data.Vector.Unboxing.Unboxable via Data.Vector.Unboxing.Generics ComplexDouble
```

## Conversion

### Conversion from/to Unboxed vector

You can use `fromUnboxedVector` and `toUnboxedVector` to convert one vector type to another.

```haskell
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Vector.Unboxing as Unboxing

convert :: Unboxed.Vector Int -> Unboxing.Vector Int
convert vec = Unboxing.fromUnboxedVector vec
```

### Coercion between Unboxing vectors

You can use `coerceVector` to convert vector types of different element types, if they have the same representation and have appropriate data constructors in scope.

```haskell
import qualified Data.Vector.Unboxing as Unboxing
import Data.MonoTraversable (ofold)
import Data.Monoid (Sum(..), All, getAll)

sum :: Unboxing.Vector Int -> Int
sum vec = getSum $ ofold (Unboxing.coerceVector vec :: Unboxing.Vector (Sum Int)) -- OK

and :: Unboxing.Vector Bool -> Bool
and vec = getAll $ ofold (Unboxing.coerceVector vec :: Unboxing.Vector All) -- fails because the data constructor is not in scope
```
