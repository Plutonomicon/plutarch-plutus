<details>
<summary> imports </summary>
<p>

```haskell
{-# LANGUAGE UndecidableInstances #-}
module Plutarch.Docs.PlutusTypePConAndPMatch () where
```

</p>
</details>

# `PlutusType`

`PlutusType` is the primary typeclass that determines the underlying representation for a Plutarch type. It lets you construct and deconstruct Plutus Core constants from a Plutarch type's constructors
(possibly containing other Plutarch terms).

```hs
class PlutusType (a :: S -> Type) where
  {-
    snip
  -}
  pcon' :: forall s. a s -> Term s (PInner a)
  default pcon' :: DerivePlutusType a => forall s. a s -> Term s (PInner a)
  pcon' = let _ = witness (Proxy @(PlutusType a)) in derivedPCon

  pmatch' :: forall s b. Term s (PInner a) -> (a s -> Term s b) -> Term s b
  default pmatch' :: DerivePlutusType a => forall s b. Term s (PInner a) -> (a s -> Term s b) -> Term s b
  pmatch' = derivedPMatch

```
> Note: You don't need to look too much into the types! After all, you'll be using `pcon` and `pmatch`, rather than `pcon'` and `pmatch'`.
> `PInner` is meant to represent the "inner" type of `a` - the Plutarch type representing the Plutus Core constant used to represent `a`.

You should always use `pcon` and `pmatch` instead of `pcon'` and `pmatch'` -
these are provided by the Prelude, and have the types you might expect:

```hs
pcon :: forall (a :: S -> Type) (s :: S) . PlutusType a => a s -> Term s a

pmatch :: forall (a :: S -> Type) (b :: S -> Type) (s :: S) . 
  PlutusType a => Term s a -> (a s -> Term s b) -> Term s b
```

Another feature of `PlutusType` instances is that you can extract out the *inner* type of any `PlutusType` instance! Above, the inner type
(or representation) of `PMaybe` was a function. You can use `pto` to safely take this inner type out-

```hs
pto :: Term s a -> Term s (PInner a)
```

This is quite useful when working with `newtype`s. Notice how `PCurrencySymbol`, for example, is simply a newtype to a `PByteString`. Its
`PInner` is also `PByteString`. To be able to use functions that operate on `PByteString`s with your `PCurrencySymbol`, you can simply take
out the `PByteString` using `pto`!

## Implementing `PlutusType` for your own types

Please see the [MLabs blog
article](https://www.mlabs.city/blog/from-term-to-script-how-plutustype-drives-plutarch),
which describes how to implement `PlutusType` instances both manually and using
derivation helpers.
