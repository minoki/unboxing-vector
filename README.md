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

Note that the vector type provided by this package (`Data.Vector.Unboxing.Vector`) is *different* from `Data.Vector.Unboxed.Vector`, although the former is a newtype for the latter.

The data constructor `Foo` can be private.

## For non-newtypes

Suppose you define a datatype isomorphic to a tuple of primitive types, like:

```haskell
data ComplexDouble = MkComplexDouble {-# UNPACK #-} !Double {-# UNPACK #-} !Double
```

In this example, `ComplexDouble` is isomorphic to `(Double, Double)`, but has a different representation. Thus, you cannot derive `Data.Vector.Unboxing.Unboxable` from `(Double, Double)`.

For such cases, unboxing-vector provides another vector type: `Data.Vector.Unboxing.Generic`.

To use this, you provide conversion functions to the isomorphic type:

```haskell
instance Data.Vector.Unboxing.Generic.Unboxable ComplexDouble where
  type Rep ComplexDouble = (Double, Double)
  from (MkComplexDouble x y) = (x, y)
  to (x, y) = MkComplexDouble x y
```

If you have the instance of `GHC.Generics.Generic`, you can omit the conversion functions:

```haskell
data ComplexDouble = ... deriving Generic

instance Data.Vector.Unboxing.Generic.Unboxable ComplexDouble
```
