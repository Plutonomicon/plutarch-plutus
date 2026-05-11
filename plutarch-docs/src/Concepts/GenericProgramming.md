<details>
<summary> imports </summary>
<p>

```haskell
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Plutarch.Docs.PMatch (Tree(..), TreeRepr) where
```

</p>
</details>

# Generic programming over Plutarch types

## Prerequisites

### `Term`

A `Term` represents Plutus Lambda Calculus expression in Plutarch world.
Allows for additional checks and safety compared to UPLC.
See more: [Plutarch Terms](../Introduction/PlutarchTerms.md).

### generics-sop

A really good introduction to `generics-sop` by the maker of the library, Andres Löh, can be found
[in this YouTube video recorded at the 2020 ZuriHac](https://www.youtube.com/watch?v=pwnrfREbhWY)

Overall image of [generics-sop](https://github.com/well-typed/generics-sop) package.

[Introduction by srid](https://srid.ca/generics-sop-intro).

Why is sum-of-products considered? It's very close to what developers think JSON is.

Generic representation of ADTs as sum of products, which can be automatically derived.
Some commonly used types/functions:

```
I - Identity functor (`newtype I a = I a`)
K - Constant functor (`newtype K a b = K a`)

Z - zero (as in Peano numbers)
S - successor (as in Peano numbers)

<https://hackage.haskell.org/package/generics-sop-0.5.1.2/docs/Generics-SOP.html#t:NS>
NS - n-ary Sum
Picking nth element of sum comes from composing Z and S

<https://hackage.haskell.org/package/generics-sop-0.5.1.2/docs/Generics-SOP.html#t:NP>
NP - n-ary Product
Value level witness for a list of types parametrized by some functor f.

SOP - sum of products
`SOP I` corresponds to haskell's structure of given SOP encoded datatype.
`SOP (Term s)` corresponds to Plutarch structure of given SOP encoded datatype.

`Code a`
The code of a datatype.

This is a list of lists of its components. The outer list contains one element per constructor. The inner list contains one element per constructor argument (field).
```

```haskell
data Tree = Leaf Int | Node Tree Tree
```

is supposed to have the following Representation:

```haskell
type TreeRepr =
  '[ '[ Int ]
   , '[ Tree, Tree ]
   ]
```

```hs
-- Generic representation of given Haskell datatype
type Rep a = SOP I (Code a)
```

This mechanism allows for generic programming over Haskell types and Plutarch types

## Intro

As Plutarch is an eDSL in Haskell, it does not allow us to work on Plutus-level variables directly.
Manipulating ADTs can be done in terms of `pcon` and `pmatch` which belong to a class called `PlutusType`.

How this class is implemented is not that important but can be looked up in `Plutarch/Internal/PlutusType.hs`
by the interested reader.

These typeclass methods could be written manually, but is a bit tedious and
error-prone, thus a generic method is also available.
Under the hood all necessary transformations are done to be able to access the data on Haskell level.

Also - as parsing data costs computation resources, it is common to pass tagged raw data until it's really needed to parse.
`PlutusType` typeclass serves 2 purposes:

1. Adds derivation via anyclass for Haskell ADTs
2. Manipulates given `S -> Type` on its internal representation (provided as type `PInner`), 
  rather than parsing/constructing the datatype back and forth.

For a fuller explanation, and several examples, please see [the MLabs blog
article](https://www.mlabs.city/blog/from-term-to-script-how-plutustype-drives-plutarch)
on `PlutusType`.

## Recommended patterns when working with pcon/pmatch

- [Tricks - Prefer matching on pmatch result immediately](../../Tricks/PreferMatchingOnPmatchResultImmediately.md)
- [Typeclasses - PlutusType, PCon, and PMatch - derive instances automatically](../../Typeclasses/PlutusType,PCon,PMatch.md)
