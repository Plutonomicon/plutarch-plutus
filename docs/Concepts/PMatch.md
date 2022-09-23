# PMatch introduction

## Prerequisites

### `Term`

A `Term` or `ClosedTerm` represents Plutus Lambda Calculus expression in Plutarch world.
Allows for additional checks and safety compared to UPLC.
See more: [Plutrach Terms](https://github.com/Plutonomicon/plutarch-plutus/blob/master/docs/Introduction/Plutarch%20Terms.md).

### Data and Scott encoding

Datatypes can be encoded using Scott and `Data` encoding.
These concepts are well explained in Plutonomicon:
[Data encoding](https://github.com/Plutonomicon/plutarch-plutus/blob/master/docs/Concepts/Data%20and%20Scott%20encoding.md#data-encoding)
and [Scott encoding](https://github.com/Plutonomicon/plutarch-plutus/blob/master/docs/Concepts/Data%20and%20Scott%20encoding.md#scott-encoding).

### `anyclass` deriving strategy

`anyclass` derivation strategy uses default implementation of given typeclass to derive an instance of it.
Usually depends that given datatype derives `Generic` typeclass also or some other too.

### generics-sop

Overall image of [generics-sop](https://github.com/well-typed/generics-sop) package.

[Introduction by srid](https://github.com/srid/generics-sop-examples/blob/master/doc/draft.md).

Why is sum-of-products considered? It's very close to what developers think JSON is.

Generic representation of ADTs as sum of products, which can be automatically derived with TH.
Some commonly used types/functions:

```
I - Identity
K - Constant

Z - zero (as in Peano numbers)
S - successor (as in Peano numbers)

<https://hackage.haskell.org/package/generics-sop-0.5.1.2/docs/Generics-SOP.html#t:NS>
NS - n-ary Sum
Picking nth element of sum comes from composing Z and S

<https://hackage.haskell.org/package/generics-sop-0.5.1.2/docs/Generics-SOP.html#t:NP>
NP - n-ary Product
Type level list of elements parametrized with additional type constuctor `f`.

SOP - sum of products
`SOP I` corresponds to haskell's structure of given SOP encoded datatype.
`SOP (Term s)` corresponds to Plutarch structure of given SOP encoded datatype.

`Code a`
The code of a datatype.

This is a list of lists of its components. The outer list contains one element per constructor. The inner list contains one element per constructor argument (field).
```

data Tree = Leaf Int | Node Tree Tree

is supposed to have the following `Code`:

```haskell
type instance Code (Tree a) =
'[ '[ Int ]
, '[ Tree, Tree ]
]
```

```haskell
-- Generic representation of given Haskell's ADT
type Rep a = SOP I (Code a)
```

All these things allow more fancy type level programming, f.e. creating a constraint that every subtype used in SOP matches some constraint:

```haskell
class (Eq a) => MyClass a

foo :: (All Eq xs) => NP f xs -> z
foo = [..]

bar :: (All MyClass xs) => NP f xs -> x
bar = foo

```

## Intro

As Plutarch is an eDSL in Haskell, it does not allow us to work on Plutus-level variables directly.
Manipulating ADTs can be done in terms of `pcon` and `pmatch`.

```haskell
class PlutusType (a :: PType) where
  -- ...
  type PInner a :: PType
  pcon' :: forall s. a s -> Term s (PInner a)
  pmatch' :: forall s b. Term s (PInner a) -> (a s -> Term s b) -> Term s b

-- | Construct a Plutarch Term via a Haskell datatype
pcon :: PlutusType a => a s -> Term s a
pcon x = ...

-- | Pattern match over Plutarch Terms via a Haskell datatype
pmatch :: PlutusType a => Term s a -> (a s -> Term s b) -> Term s b
pmatch x = ...
```

These functions could be written manually, but is a bit tedious and error-prone, thus generic representation from `generics-sop` is used.
Under the hood all necessary transformations are done to be able to access the data on Haskell level.

Also - as parsing data costs computation resources, it is common to pass tagged raw data until it's really needed to parse.
`PlutusType` typeclass serves 2 purposes:

1. Adds derivation via anyclass for Haskell ADTs
2. Manipulates given `PType` on its internal representation (provided as type `PInner`), rather than parsing/constructing the datatype back and forth.

`pcon'` and `pmatch'` are coerced to `pcon` and `pmatch`.

Examples on how to derive `PlutusType` to either Data or Scott encoding:

```haskell
{- |
  > {-# LANGUAGE DeriveAnyClass        #-}
  > {-# LANGUAGE DeriveGeneric         #-}
  > {-# LANGUAGE DerivingStrategies    #-}
  > {-# LANGUAGE LambdaCase            #-}
  > {-# LANGUAGE DataKinds             #-}
  > {-# LANGUAGE PartialTypeSignatures #-}
  > {-# LANGUAGE ScopedTypeVariables   #-}
  > {-# LANGUAGE TypeFamilies          #-}
  > {-# LANGUAGE TypeOperators         #-}
  > import qualified GHC.Generics as GHC
  > import qualified Generics.SOP as SOP
  > import Plutarch
  >
  > data MyType (a :: PType) (b :: PType) (s :: S)
  >   = One (Term s a)
  >   | Two (Term s b)
  >   deriving stock (GHC.Generic)
  >   deriving anyclass (SOP.Generic, PlutusType)
  > instance DerivePlutusType (MyType a b) where type DPTStrat _ = PlutusTypeScott

  If you instead want to use data encoding, you should derive 'PlutusType' and provide data strategy:

  > data MyTypeD (a :: PType) (b :: PType) (s :: S)
  >   = One (Term s (PDataRecord '[ "_0" ':= a ]))
  >   | Two (Term s (PDataRecord '[ "_0" ':= b ]))
  >   deriving stock (GHC.Generic)
  >   deriving anyclass (SOP.Generic, PlutusType)
  > instance DerivePlutusType (MyType a b) where type DPTStrat _ = PlutusTypeData

  Alternatively, you may derive 'PlutusType' by hand as well. A simple example, encoding a
  Sum type as an Enum via PInteger:

  > data AB (s :: S) = A | B
  >
  > instance PlutusType AB where
  >   type PInner AB _ = PInteger
  >
  >   pcon' A = 0
  >   pcon' B = 1
  >
  >   pmatch' x f =
  >     pif (x #== 0) (f A) (f B)
  >

  instead of using `pcon'` and `pmatch'` directly,
  use 'pcon' and 'pmatch', to hide the `PInner` type:

  > swap :: Term s AB -> Term s AB
  > swap x = pmatch x $ \\case
  >   A -> pcon B
  >   B -> pcon A

```

`Maybe` manually encoded in both ways:

```haskell
-- | Scott
data PSMaybe a s = PSJust (Term s a) | PSNothing

-- | Newtype wrapper around function that represents Scott encoding,
-- | Plutarch uses generic one for deriving.
newtype ScottEncodedMaybe a b s = ScottEncodedMaybe (Term s ((a :--> b) :--> PDelayed b :--> b))

instance PlutusType (ScottEncodedMaybe a r) where
  type PInner (ScottEncodedMaybe a r) = (a :--> r) :--> PDelayed r :--> r
  pcon' (ScottEncodedMaybe x) = x
  pmatch' x f = f (ScottEncodedMaybe x)

instance PlutusType (PSMaybe a) where
  -- The resulting type of pattern matching on Maybe is quantified via `PForall`
  type PInner (PSMaybe a) = PForall (ScottEncodedMaybe a)
  pcon' (PSJust x) = pcon $ PForall $ pcon $ ScottEncodedMaybe $ plam $ \f _ -> f # x
  pcon' PSNothing = pcon $ PForall $ pcon $ ScottEncodedMaybe $ plam $ \_ g -> pforce g
  pmatch' x' f =
    pmatch x' $ \(PForall sem) ->
      pmatch sem $ \(ScottEncodedMaybe x) ->
        x # plam (f . PSJust) # pdelay (f PSNothing)

-- | Maybe encoded using Constr
data PMaybeData a (s :: S)
  = PDJust (Term s a)
  | PDNothing

-- | Note - thing hold in PMaybeData must be able to be represented as Data too, not needed in case of Scott version
instance PIsData a => PlutusType (PMaybeData a) where
  type PInner (PMaybeData a) = PData
  pcon' (PDJust x) = pforgetData $ pconstrBuiltin # 0 #$ psingleton # pforgetData (pdata x)
  pcon' PDNothing = pforgetData $ pconstrBuiltin # 1 # pnil
  pmatch' x f = (`runTermCont` f) $ do
    constrPair <- TermCont $ plet (pasConstr # x)
    indexNum <- TermCont $ plet (pfstBuiltin # constrPair)
    TermCont $ \g -> pif (indexNum #== 0)
        (g $ PDJust $ punsafeCoerce $ phead # (psndBuiltin # constrPair))
        (pif (indexNum #== 1)
          (g PDNothing)
          perror
        )

```

### Generic derivation of PCon/PMatch

The mechanism of `PlutusType` derivation relies heavily on generic representation of ADT as sum-of-products.
Very high level overview:
For `pmatch`:

- Scott encoding - for each `sum` branch, create a corresponding `plam` handler
- Data encoding - for each `sum` branch, apply each element of list in `Constr` to a handler

For `pcon`:

- Scott encoding - encode data type as lambda
- Data encoding - create a `Constr` with corresponding number of constructor

Up to these - there's type level programming done to make things type safe and generic.

## Recommended patterns when working with pcon/pmatch

[Tricks - Prefer matching on pmatch result immediately](https://github.com/Plutonomicon/plutarch/blob/master/docs/Tricks/Prefer%20matching%20on%20pmatch%20result%20immediately.md)
[Typeclasses - PlutusType, PCon, and PMatch - derive instances automatically](https://github.com/Plutonomicon/plutarch/blob/master/docs/Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md)
