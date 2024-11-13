<details>
<summary> imports </summary>
<p>

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Plutarch.Docs.PlutarchConstants (x, s, i, xd, hexs, justTerm, hPJustPInteger) where 
import Plutarch.Prelude
```

</p>
</details>
# Plutarch Constant `Term`s

When evaluated, a constant Plutarch `Term` will always yield the same result. There are several ways of building constant `Term`s:

- Statically building constant `Term`s from concrete Haskell values when we know the value at compile-time.
- Dynamically building constant `Term`s from Haskell values, i.e. when the constant produced depends on a dynamic value.
- Overloaded literal syntax
- Helper functions

## Static building of constant `Term`s with `pconstant`

If we know the desired value of a constant `Term` at compile-time, we can build the `Term` directly from [Haskell synonyms](./../../Concepts/Haskell%20Synonym.md). The function to do so is `pconstant`.

Constructing constants in this way utilizes the [`PConstant`/`PLift`](./../../Typeclasses/PConstant%20and%20PLift.md) typeclasses. These typeclasses expose the following [associated type families](https://wiki.haskell.org/GHC/Type_families#An_associated_type_synonym_example):

```hs
type PLifted :: PType -> Type

type PConstanted :: Type -> PType
```

`pconstant` takes a single argument: a regular Haskell type with a `PConstant`/`PLift` instance, and yields a Plutarch term tagged with the corresponding Plutarch type.

The relation between the Plutarch type and its Haskell synonym is established by the type families. For any Haskell type `h`, `PConstanted h` is the corresponding Plutarch type. Similarly, for any Plutarch type `p`, `PLifted p` corresponds to the Haskell synonym.

Lawful instances shall obey the following invariants:

```hs
PLifted (PConstanted h) ~ h
PConstanted (PLifted p) ~ p
```

For example:

```haskell
-- | A Plutarch level boolean. Its value is "True", in this case.
x :: Term s PBool
x = pconstant True
```

The familiar `Bool` has a `PConstant` instance and it corresponds to `PBool` (which has a `PLift` instance). Therefore `PLifted PBool ~ Bool` and `PConstanted Bool ~ PBool`.

You can also directly create a [`PAsData`](./../../Types/PAsData.md) term using `pconstantData`:

```haskell
-- | A Plutarch level boolean encoded as `Data`.
xd :: Term s (PAsData PBool)
xd = pconstantData True
```

## Dynamic building of constant `Term`s with `pcon`

Sometimes the value that we want to treat as a constant `Term` is not known at compile time. To explain how to construct constants when we can only determine the value at runtime, we will examine the `PMaybe` Plutarch type. It can serve the same purpose as the `Maybe` type in Haskell: to represent the situation where computation may not produce a sensible result.

`PMaybe` has the following definition:

```hs
data PMaybe (a :: PType) (s :: S)
  = PJust (Term s a)
  | PNothing
```

and the following kind:

```hs
>>> :k PMaybe
PMaybe :: PType -> S -> Type
```

Let's dissect what this means.

- `PMaybe` builds a `PType` from a `PType`; given a `PType`, we can tag a computation with the type `PMaybe a` to indicate that its return value should be semantically either `Just a` or `Nothing`. Such a tagging would look like a value with the type `Term s (PMaybe a)`.
- `PJust` and `PNothing` are data constructors. They are _not_ tags. `PJust :: Term s a -> PMaybe (a :: PType) (s :: S)` is a helper to signify the concept of `Just x`. It contains a Plutarch term.

Now suppose that we want to carry around a constant `Term` in a Plutarch script that can be either `PJust a` or `PNothing`. To do so, we need a function to go from `PJust a` (which we _can_ instantiate as a Haskell value, unlike `PInteger`) to a `Term s (PMaybe a)`. This function is `pcon`:

```haskell
-- pcon :: a s -> Term s a
-- For example:

x' :: Term s PInteger
x' = pconstant 3

justTerm :: Term s (PMaybe PInteger)
justTerm = pcon (PJust x')
```

These types deserve some explanation.

- We are familiar by now with the type of `x`; it is a computation that returns a value that can be interpreted as a Haskell integer if evaluated successfully (in this case, 3).
- The type of `justTerm` represents a computation tagged with the `PMaybe PInteger` type.

That is, if we ask `justTerm` what it will return when evaluated, it responds, "You should interpret the value I give you as either `Nothing` or `Just Integer`." Of course, we know that the result will always be `Just 3`; but this is the general mechanism to declare a function requiring a `Maybe`.

If you don't want to pretend to not know `x` during compile time, another example may be:

```haskell
hPJustPInteger :: Term s PInteger -> Term s (PMaybe PInteger)
hPJustPInteger x = pcon (PJust x)
```

The `pcon` function is a method of the [`PCon` typeclass](./../../Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md).

## Overloaded literals

`pconstant` and `pcon` are the long-form ways of building constants. Specific constant Haskell literals are overloaded to help construct Plutarch constants. We provide two examples below.

```haskell
-- | A Plutarch level integer. Its value is 1, in this case.
i :: Term s PInteger
i = 1

-- | A Plutarch level string (this is actually `Text`). Its value is "foobar", in this case.
s :: Term s PString
s = "foobar"
```

## Helper functions

Finally, Plutarch provides helper functions to build certain types of constants:

```haskell
-- | A plutarch level bytestring. Its value is [65], in this case.
hexs :: Term s PByteString
hexs = phexByteStr "41"
-- ^ 'phexByteStr' interprets a hex string as a bytestring. 0x41 is 65 - of course.
```
