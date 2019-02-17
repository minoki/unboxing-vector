# unboxing-vector

A newtype-friendly wrapper for `Data.Vector.Unboxed` in [`vector` package](http://hackage.haskell.org/package/vector).

## Description

Suppose you define a newtype for `Int` and want to store them in an unboxed vector.

```haskell
newtype Foo = Foo Int

generate 10 (\i -> Foo i) :: Vector Foo
```

With plain `Data.Vector.Unboxed`, you either write two dozen of lines of code to get it work (the exact code is [here](test/Foo.hs)), or resort to Template Haskell ([`vector-th-unbox` package](http://hackage.haskell.org/package/vector-th-unbox)).

But with `Data.Vector.Unboxing`, the code you write is just two lines:

```haskell
instance Data.Vector.Unboxing.Unboxable Foo where
  type Underlying Foo = Int
```

Note that the vector type provided by this package (`Data.Vector.Unboxing.Vector`) is *different* from `Data.Vector.Unboxed.Vector`, although the former is a newtype for the latter.
