> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high-quality documentation for Plutarch users!

<details>
 
<summary> Table of Contents </summary>
 
- [Overview](#overview)
- [Untyped Plutus Core (UPLC)](#untyped-plutus-core-uplc)
- [Plutarch Types](#plutarch-types)
- [Plutarch `Term`s](#plutarch-terms)
  * [Plutarch Constant `Term`s](#plutarch-constant-terms)
    + [Static building of constant `Term`s with `pconstant`](#static-building-of-constant-terms-with-pconstant)
    + [Dynamic building of constant `Term`s with `pcon`](#dynamic-building-of-constant-terms-with-pcon)
      - [Pattern matching `Term`s on `PType`s with `pmatch`.](#pattern-matching-terms-on-ptypes-with-pmatch)
      - [`class (PCon a, PMatch a) => PlutusType (a :: PType)` -- with a default instance.](#class-pcon-a-pmatch-a--plutustype-a--ptype----with-a-default-instance)
    + [Overloaded literals](#overloaded-literals)
    + [Helper functions](#helper-functions)
  * [Lambdas; Plutarch-level Function `Term`s.](#lambdas-plutarch-level-function-terms)
    + [Function Application](#function-application)
    + [Strictness and Laziness; Delayed Terms and Forcing](#strictness-and-laziness-delayed-terms-and-forcing)
- [References](#references)

</details>

# Overview

Plutarch is an eDSL in Haskell for writing on-chain scripts for Cardano. With some caveats, Plutarch is a [simply-typed lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus) (or STLC). Writing a script in Plutarch allows us to leverage the language features provided by Haskell while retaining the ability to compile to compact Untyped Plutus Core (or UPLC, which is an untyped lambda calculus).

When we talk about "Plutarch scripts," we are referring to values of type `Term (s :: S) (a :: PType)`. `Term` is a `newtype` wrapper around a more complex type, the details of which Plutarch end-users can ignore. A `Term` is a typed lambda term; it can be thought of as representing a not-yet-evaluated computation that, if successfully evaluated, will return a value of type `a`. 

The two type variables of the `Term s a` declaration have particular kinds: 

- `s :: S` is like the `s` of `ST s a`. It represents the computation context in a manner that mimics mutable state while providing a familiar functional interface. Sections 1 through 4 of [[1]](#references) give an accessible introduction to how this works. `s` is never instantiated with a concrete value; it is merely a type-level way to ensure that computational contexts remain properly encapsulated (i.e., different state threads don't interact). For more in-depth coverage of this and other eDSL design principles used in Plutarch, see [[2]](#references).
- `a :: PType` is short-hand for "Plutarch Type". We prefix these types with a capital `P`, such as `PInteger`, `PBool`, and so forth. _Tagging_ a `Term` with a `PType` indicates the type of the `Term`'s return value. Doing this allows us to bridge between the simple type system of Plutarch and the untyped UPLC.

Note that we _should not_ think of a type of kind `PType` as carrying a value; it is a tag for a computation that may produce a value. For instance, the definition of `PInteger` is simply

```hs
data PInteger s
```

That is, there are no data constructors. _If_ a value of type `Term s PInteger` successfully executes the computation within context `s`, the value _computed_ will be an integer. We will never encounter values such as `y :: PInteger; y = 3`; they simply do not exist. While readers new to Plutarch may need some time to fit this into their mental model, it is a crucial distinction to keep in mind.

> For brevity, we will say a "value of type `Term s a` will evaluate to (...)". This phrase will carry two implicit notions: one, that `Term s a` represents a computation executed in the context `s`; two, evaluating `Term s a` is not guaranteed to succeed.

In brief, when writing Plutarch scripts, we have a few tasks:

- A.) Defining _Plutarch Types_ (or _`PType`s_). We prefix these types with a capital `P`, such as `PInteger`, `PMaybe a`, `PBool`, and so forth. As previously mentioned, these form the "tags" for Plutarch `Term`'s, representing the type of the result of compiling and evaluating a Plutarch Script.
- B.) Working with _Plutarch `Terms`_, which are values of the type `Term (s :: S) (a :: PType)`. These are the Plutarch scripts themselves, from which we build up more complex scripts before compiling and executing them on-chain.
- C.) Writing Haskell-level functions _between Plutarch Terms_ (i.e., with types like `Term s a -> Term s b`). Doing so allows us to leverage Haskell's language features and ecosystem.
- D.) Efficiently Converting the functions from _(C.)_ to _Plutarch-level functions_, which are of the type `Term s (a :--> b)`. We can _directly_ convert the functions from (C.) to Plutarch-level functions at the most naive level using `plam`. Additional Plutarch utilities provide for optimization opportunities.
- E.) Compiling and executing the functions from _(D.)_, targetting UPLC for on-chain usage. 

As a preview, the bridge Plutarch provides between Haskell and UPLC looks something like this:

```
------------------------------------------------------
|                  *Haskell World*                   |
------------------------------------------------------
| Values with types like `Bool`, `Integer`, `Maybe a`|
------------------------------------------------------
                           ^                     | 
 (functions like `plift`)--|                     |--(functions like `pconstant`) 
                           |                     |
                           |                     v         (`pcon`)
-------------------------------------------------------    |                     -------------------------------------------------------
|                  *Plutarch Term World*              | <----------------------- |                  *Plutarch Type World*              |
-------------------------------------------------------                          -------------------------------------------------------
| STLC terms; constants like `Term s PInteger` and    | -----------------------> | Types like `PInteger`, `PMaybe a`                   |
| lambdas like `Term s (PInteger :--> PBool)`         |        |                 |                                                     |
-------------------------------------------------------        (`pmatch`)       -------------------------------------------------------
                              ^                     | 
(functions like `pfromData`)--|                     |
                              |                     |--(functions like `ptoData`,
                              |                     |   types like `AsData a`) 
                              |                     |
                              |                     v
-------------------------------------------------------
|                    *UPLC World*                     |
-------------------------------------------------------
| Untyped lambda calculus terms. Values of type `Data`|
|                                                     |
-------------------------------------------------------
```

This document covers the bridge between Haskell and Plutarch at a high level. It will _not_ cover all techniques necessary to write production-ready scripts. Nor will it cover the bridge between Plutarch and UPLC beyond the minimum. Nonetheless, it should provide sufficient background to prepare the reader for further study.

# Untyped Plutus Core (UPLC)

Plutarch compiles to UPLC. Most Plutarch end-users will not need to concern themselves with the details of UPLC, but a brief overview will aid in building a mental model of how Plutarch works.

Unlike Haskell, UPLC is a low-level and untyped language implementing a basic lambda calculus. Consequently, it supports only a handful of built-in values and functions which may be strung together in lambda applications. The built-in types provided by UPLC include the usual primitive types -- integers, byte strings and strings, booleans, and so forth -- and a special `Data` value that can encode representations of arbitrary sum-of-products Haskell types.

While the _semantic_ meaning of a Haskell type such as `Maybe Integer` is missing in UPLC, it still can be _represented_ in UPLC through certain [encodings](./CONCEPTS.md#data-encoding-and-scott-encoding]. These encodings are the aformentioned `Data` builtin, which can be used to represent arbitrary types in on-chain components such as Datum and Redeemers as well as Scott Encoding, which can additionally encode function types but cannot be used in Datums or Redeemers. The key idea is that UPLC doesn't track what differentiates semantically distinct values, regardless of their encoding, and will not prevent a programmer from operating on the underlying representation in non-sensical ways. 

Plutarch's solution is to _tag_ scripts that compile to UPLC (i.e., Plutarch ` Term`s) with types. Doing so allows the Plutarch compiler to track and type check operations on semantically distinct UPLC values. These tags are provided by "Plutarch Types", or "types of kind `PType`". 

For the Plutarch compiler to bridge between arbitrary, semantically-rich Haskell types and the untyped values of UPLC, it is necessary to associate various bits of information with `PType`s. On the one hand, each `PType` should have some semantic, type-level richness such as typeclass instances (otherwise, there would be little point in programming in Haskell!). On the other hand, each `PType` needs to have a UPLC representation, either as a built-in primitive value,`Data`, or as a Scott-encoded lambda, in order to compile to UPLC.

# Plutarch Types

When this guide uses the term "Plutarch Type," we explicitly talk about a type of _kind_ `PType`. We will refer to  _" types of kind `PType` "_ simply as" `PType`s." We explicitly qualify when referring to the _kind_ `PType`.

`PType` is defined as `type PType = S -> Type`; that is, it is a _kind synonym_ for `S -> Type` (where `S` and `Type` are themselves kinds). This synonym is important to keep in mind because when querying the kind of something like `PBool` in, say, GHCi, we will _not_ see `PType` as the kind. Instead, we get 

```hs
ghci> :k PBool
PBool :: S -> Type
```

Thus, any time we see the kind `S -> Type`, we should mentally substitute its kind synonym `PType`. We reiterate: types of kind `PType`, should be considered as _tags_ on computation. They do not represent types of values in the same way as standard Haskell types. 

The kind of basic types such as `Integer` in Haskell is `Type`; the corresponding "basic" kind in Plutarch is simply `PType`. Higher-kinded types in Haskell, such as `Maybe`, will kinds such as `Type -> Type`. In Plutarch, the corresponding kind is:

```hs
ghci> :k PMaybe
PMaybe :: PType -> S -> Type
```

Since the kind arrow `->` is right-associative, we first read this as `PMaybe :: PType -> (S -> Type)`; and since we know that that `PType` and `S -> Type` and synonyms, we read this as `PMaybe :: PType -> PType`, which should agree without intuition.

The kind `S -> Type` is mysterious at first, but we recall that `PType`s are _tags_ on (unexecuted) computations indicating their result type. The `S` kind represents the computational context; thus, a `PType` expects to receive a _computational context_ represented by a value `s` whose type has kind `S` that it will tag to produce a `Type`. Note that end-users never instantiate the value `s` with a concrete value; it is simply a type-level mechanism to maintain functional purity.

The above notion is essential to understanding why not all `PType`s have data constructors; the data constructors are irrelevant, except insofar as they enable the implementation to keep track of Haskell-level and UPLC-level representations. `PInteger` is one such case; it is impossible to construct a constant `y` where `y = PInteger`. Other `PType`s, such as `PMaybe`, _do_ have data constructors (specifically `PJust` and `PNothing`), but _still_ do not carry data from the viewpoint of UPLC. A value such as `PNothing` merely facilitates convenient term construction and deconstruction. When `pcon` sees `PNothing`, it knows it should build a UPLC constant that is _morally_ equivalent to the concept of `Nothing :: Maybe a`.

# Plutarch `Term`s

Plutarch `Term`s are terms in the sense of simply-typed lambda calculus terms. In a lambda calculus, we can construct terms as either "constants" or "lambdas," and terms can either be "open" (having free variables) or "closed" (having no free variables). We compose Plutarch `Term`s to build up increasingly complex computations. Once all free variables are eliminated from a `Term` (making it a `Closed Term`), we can compile it using the eponymous function from the `Plutarch` module:

```hs
-- | Closed term is a type synonym 
type ClosedTerm (a :: PType) = forall (s :: S). Term s a 

-- | Compile operates on closed terms to produce usable UPLC scripts.
compile :: ClosedTerm a -> Script
```
`Term`s are constructed from Haskell values and are tagged with `PType`s.

## Plutarch Constant `Term`s

When evaluated, a constant Plutarch `Term` will always yield the same result. There are several ways of building constant `Term`s:

- Statically building constant `Term`s from concrete Haskell values when we know the value at compile-time. 
- Dynamically building constant `Term`s from Haskell values, i.e., when the constant produced depends on a function argument
- Overloaded literal syntax
- Helper functions
  
### Static building of constant `Term`s with `pconstant`

If we know the desired value of a constant `Term` at compile-time, we can build the `Term` directly using [Haskell synonyms](./CONCEPTS.md#haskell-synonym-of-plutarch-types). The function to do so is `pconstant`. 

Constructing constants in this way utilizes [associated type familes](https://wiki.haskell.org/GHC/Type_families#An_associated_type_synonym_example) provided in the `PConstant` and `PLift` typeclasses. On the Haskell <-> Plutarch side, the basic flow is this:

- `pconstant` takes a single argument. The type of That argument must be an instance of the `PUnsafeLiftDecl` typeclass.
- The `PUnsafeLiftDecl` has a superclass, `PConstant`. This class (among other things) associates a type family `PConstanted h :: PType` to a Haskell type `h :: Type`.
- This means that `pconstant` can take a value `x` of type `Type` and produce a type of kind `PType`. `pconstant` to embeds the value `x` into a `Term s a`, where `a` is the `PType` associated with the type of `x`. 
- The computation represented by `Term s a` is now tagged with the appropriate type.

On the Plutarch <-> UPLC side, the flow is as follows:

- The `PConstant` class associates a second type family, `PConstantRepr`, with the Haskell type `h :: Type`. This type family gives the UPLC representation of `h`. 
- When we construct a `Term` as above and compile it, the type family guides the compiler to the on-chain UPLC representation.

The end-user effect `pconstant` always takes in a regular Haskell value with a `PLift`, and thus `PConstant` typeclass instances (along with the other superclasses of each) to create its Plutarch synonym. These typeclasses may seem burdensome, but they are provided by the Plutarch libraries for most basic types and can be automatically derived in many other cases. For example:

```hs
import Plutarch.Prelude

-- | A Plutarch level boolean. Its value is "True", in this case.
x :: Term s PBool
x = pconstant True
```

To reiterate: in the above snippet, `x` represents a computation that is _tagged_ with the type `PBool`, that, when evaluated, returns a result that is semantically `True`. The `PLift` and `PConstant` typeclasses provided the associated types necessary for Plutarch to convert to and from Haskell types to Plutarch's `PType` tags. 

### Dynamic building of constant `Term`s with `pcon`

Sometimes the value that we want to treat as a constant `Term` is not known at compile time. To explain how to construct constants when we can only determine the value at runtime, we will examine the `PMaybe` Plutarch type. It can serve the same purpose as the `Maybe` type in Haskell: to represent the situation where computation may not produce a sensible result. 

`PMaybe` has the following definition:

```hs
data PMaybe (a :: PType) (s :: S)
  = PJust (Term s a)
  | PNothing
```
and the following kind:
```hs
ghci> :k PMaybe
PMaybe :: PType -> S -> Type
```

Let's dissect what this means.

- `PMaybe` builds a `PType` from a `PType`; given a `PType`, we can tag a computation with the type `PMaybe a` to indicate that its return value should is semantically either `Just a` or `Nothing`. Such a tagging would look like a value with the type `Term s (PMaybe a)`.
- `PJust` and `PNothing` are data constructors. They are _not_ tags. `PJust :: Term s a -> PMaybe (a :: PType) (s :: S)` is a _wrapper_ around a computation that can return a result of type `a`. 

Now suppose that we want to carry around a constant `Term` in a Plutarch script that can be either `PJust a` or `PNothing`. To do so, we need a function to go from `PJust a` (which we _can_ instantiate as a Haskell value, unlike `PInteger`) to a `Term s (PMaybe a)`. This function is `pcon`: 

```hs
pcon :: a s -> Term s a

-- For example:

x :: Term s PInteger
x = pconstant 3

justx :: forall {s :: S}. PMaybe (PInteger @{S}) s
justx = PJust x

justxTerm :: forall {s :: S}. Term s (PMaybe (PInteger @{S}))
justxTerm = pcon justx
```

These types deserve some explaination. 

- We are familiar by now with the type of `x`; it is a computation that returns a value that can be interpreted as a Haskell integer if evaluated successfully (in this case, 3).
- The type of `justx` says the following: "a computation tagged with this type indicates that the computation, evaluated in the context `s`, will  produce a value to be interpreted as either  `Just Integer` or `Nothing`."
- The type of `justxTerm` represents a computation tagged with the type of `justx`.

That is, if we ask `justxTerm` what it will return when evaluated, it responds, "You should interpret the value I give you as either `Nothing` or `Just Integer`." Of course, we know that the result will always be `Just 3`; but this is the general mechanism to declare a function requiring a `Maybe`. 

The `pcon` function is a method of the [`PCon` typeclass](./TYPECLASSES.md#plutustype-pcon-and-pmatch).
    
#### Pattern matching `Term`s on `PType`s with `pmatch`.

We've shown how to construct `Term`s out of the data constructors of types with kind `PType` (i.e., `pcon . PJust`). Next, it is natural that we may want to pattern match on `Term` with a known `PType` tag (i.e., of a value with type `Term s (PMaybe a)`) to produce another `Term` (i.e., depending on whether the value matches `PJust _` or `Nothing`.)

The function that we need is a method of the `PMatch` typeclass. For the time being, we will ignore the details of implementation and only look at the type:

```hs
pmatch :: forall (a :: PType) (s :: S) (b :: PType).
    PMatch a =>               {- We have two constraints on the type `a`: 
                                  it has a `PMatch` instance and is of kind `PType`.-}
    Term s a ->               -- Given a `Term` tagged with `a`...
    (a s -> Term s b) ->      -- ...and a function from a type of kind `PType` to a Term s b)`...
    Term s b                  -- ...produce a `Term s b`
```

The annotation of the second argument deserves some focus; the second argument has its type displayed as `(a s -> Term s b)`, but our comment suggests something else. First, recall that `a` is declared to have kind `PType`, and `PType` is a kind synonym for `S -> Type`. Thus, since `s` has kind `S`, we have that `a s` has the _kind_ `PType`.

What this means, in practice, is that `pmatch` matches on the possible values of the _result_ of evaluating a `Term s a` -- specifically, it matches on _values_ of a _type_ that have _kind `PType`_ -- and branches accordingly. The second argument to `pmatch` is a _continuation_; it determines how the program continues once `pmatch` has done its work.

We have already introduced a type with kind `PType` suitable for branching. Here is an example of a :

```hs
{- | This function takes in a Haskell-level `PMaybe` value (specifically, _not_ a `Term`)
     and return a `Term` depending on the Haskell-level pattern match on `PMaybe`s data
     constructors.
-}
continuation :: forall {a :: PType} {s :: S}. PMaybe a s -> Term s PBool
continuation x = case x of
   PJust _ -> pconstant True
   PNothing -> pconstant False

{- | A Haskell-level `isJust` on Plutarch `Term`s. `pmatch` can match on
     the possibilities of `PJust _` or `PNothing` being the result of an evaluated
     `Term`.
-}
hisJust :: forall {a :: PType} {s :: S}. Term s (PMaybe a) -> Term s PBool
hisJust x = pmatch x continuation

-- | A Plutarch-level `isJust`
pisJust :: forall {s :: S} {a :: PType}. Term s (PMaybe a :--> PBool)
pisJust = plam hisJust
```
Readers should note that this is not the most ergonomic way to deal with pattern matching (Plutarch provides two versions of `do` syntax), but it _is_ how the more ergonomic methods work under the hood.

#### `class (PCon a, PMatch a) => PlutusType (a :: PType)` -- with a default instance.
Combining the `PCon` and `PMatch` typeclasses (providing `pcon` and `pmatch`, respectively) is sufficient to encode Haskell types as UPLC types. 
Having both a `PCon` and `PMatch` instance means that the `PlutusType` instance -- the instance detailing how to go from Plutarch `Term`s to UPLC representations -- can be derived generically!

While we won't go into detail here, this is a non-trivial fact that deserves explicit mention; we've gone from Haskell types to Plutarch `PType` tags, tagged Plutarch `Terms` and back, and obtained a mechanism to represent UPLC terms for free.

### Overloaded literals

`pconstant` and `pcon` are the long-form ways of building constants. Specific constant Haskell literals are overloaded to help construct Plutarch constants. We provide two examples below.

```hs
{-# LANGUAGE OverloadedStrings #-}

import Plutarch.Prelude

-- | A Plutarch level integer. Its value is 1, in this case.
x :: Term s PInteger
x = 1

-- | A Plutarch level string (this is actually `Text`). Its value is "foobar", in this case.
y :: Term s PString
y = "foobar"
```

### Helper functions

Finally, Plutarch provides helper functions to build certain types of constants:

```hs
import qualified Data.ByteString as BS
import Plutarch.Prelude

-- | A plutarch level bytestring. Its value is [65], in this case.
x :: Term s PByteString
x = phexByteStr "41"
-- ^ `phexByteStr` interprets a hex string as a bytestring. 0x41 is 65 - of course.
```

## Lambdas; Plutarch-level Function `Term`s.

Lambdas are the second form of Plutarch `Term`s. Lambda terms are represented at the type level by the infix type constructor `:-->`; a value of type `Term s (a :--> b)` evaluates to a function that takes a value of type `a` and produces a value of type `b`.

You can create Plutarch lambda `Term`s by applying the `plam` function to a Haskell-level function that works on Plutarch terms. The true type of `plam` itself is unimportant to end-users of Plutarch, but it should be thought of as 

```hs
plam :: (Term s a -> Term s b) -> Term s (a :--> b)
```

To create the identity function as a Plutarch lambda, we would thus use:

```hs
-- | Haskell-level `id` function specialized to the `Term s a` type``
termId :: Term s a -> Term s a
termId x = x

-- | Plutarch-level `id` lambda
pid :: Term s (a :--> a)
pid = plam x

-- | Equivalently:
pid' :: Term s (a :--> a)
pid' = plam $ \x -> x
```

Notice the type. A Plutarch lambda `Term` uses the `:-->` infix operator to encode a function type. So in the above case, `pid` is a Plutarch level function that takes a type `a` and returns the same type. As one would expect, `:-->` is right-associative, and things curry like a charm.

Guess what this Plutarch level function does:

```hs
f :: Term s (PInteger :--> PString :--> a :--> a)
```

It takes in an integer, a string, and a type `a` and returns the same type `a`. Notice that the types are all of kind `PType`. This means that when faced with filling out the gap:

```hs
f :: Term s (PInteger :--> PString :--> a :--> a)
f = plam $ \???
```

We know that the argument to `plam` here is a Haskell function `g` with type `Term s PInteger -> Term s PString -> Term s a -> Term s a`.

### Function Application

Once we construct a Plutarch lambda `Term` using `plam`, it is rather useless unless we apply it to an argument. Plutarch provides two operators to do so 

```hs
{- |
  High precedence infixl function application, to be used like
  function juxtaposition. e.g.:

  >>> f # x # y
  f x y
-}
(#) :: Term s (a :--> b) -> Term s a -> Term s b
infixl 8 #

{- |
  Low precedence infixr function application, to be used like
  `$`, in combination with `#`. e.g.:

  >>> f # x #$ g # y # z
  f x (g y z)
-}
(#$) :: Term s (a :--> b) -> Term s a -> Term s b
infixr 0 #$
```

The types of each operator match our intuition. Applying a lambda `Term` to a `Term` (tagged with the `PType` of the domain of the lambda) produces a `Term` (tagged with the `PType` of the codomain.).

### Strictness and Laziness; Delayed Terms and Forcing

Plutarch, like UPLC, is strict by default; this is in contrast to Haskell, which is nonstrict. In practice, this means that calling a function in Plutarch evaluates _all_ arguments to the Plutarch lambda `Term` beforehand.

> Note: the below example does not correspond precisely to the implementation of `pif` or `pif'`; it is for didactic purposes only

This behavior may be undesirable, for example, when one of two `Term`s are branched upon within an `if` statement. In Plutarch, `pif'` is strict and `pif` is lazy. A strict `pif'` could look something like 

```hs
{- | Haskell-level `if` function between `Term`s. This  function is nonstrict;
only a single branch is evaluated (since this is a Haskell function).
Unfortunately, it _doesn't_ translate to a non-strict Plutarch lambda, 
as we will see below.
-}
hif' :: Term s PBool -> Term s a -> Term s b -> Term s c
hif' cond whenTrue whenFalse = pmatch cond $ \case
  PTrue -> whenTrue
  PFalse -> whenFalse

{- | Strict Plutarch-level `if`. The arguments _are_ evaluated
(by default), meaning that both branches are evaluated.
pif' :: forall {s :: S} {b :: PType}. Term s (PBool :--> (b :--> (b :--> b)))
pif' = plam hif
```

A strict `if` is undesirable for the obvious reason: we only follow one branch at runtime, so it doesn't make sense to evaluate both before examining the `PBool` value to which we apply `pif`.

To avoid this, we use `pdelay` to create a "delayed `Term`." `pdelay` wraps the `PType` tag of a term, overriding the default strict behavior and indicating that the term should _not_ be evaluated immediately. `pdelay` has the following type:

```hs
pdelay :: Term s a -> Term s (PDelayed a)
```

A delayed term evaluates when it is _forced_ using the `pforce` function. Forcing a term strips the `PDelayed` wrapper:

```hs
pforce :: Term s (PDelayed a) -> Term s a 
```

Thus, if we wanted a lazy `pif", we could do the following:

```hs
{- | Haskell-level `if` function between `Term`s. This time
it _does_ translate to a lazy Plutarch lambda.
-}
hif :: Term s PBool -> Term s a -> Term s a -> Term s a
hif cond whenTrue whenFalse = pforce $ pif' # cond # pdelay whenTrue # pdelay whenFalse

{- | Plutarch-level `if`. We delay the arguments in `hif` so they are not applied
until forced.
-}
pif :: forall {s :: S} {b :: PType}. Term s (PBool :--> (b :--> (b :--> b)))
pif = plam pif'
```

A note of caution: calling `pforce` on the same delayed term twice will execute the computation each time. Users familiar with Haskell's handling of laziness -- where forcing a thunk twice never duplicates computation -- should note that this UPLC behaves differently.

Finally, readers should note that `pdelay` and `pforce` are extremely powerful tools when writing Plutarch scripts and are encouraged to familiarize themselves accordingly. 


# References 

- [1] [Lazy Functional State Threads, by John Launchbury and Simon L Peyton Jones](https://www.microsoft.com/en-us/research/wp-content/uploads/1994/06/lazy-functional-state-threads.pdf) 
- [2] [Unembedding Domain-Specific Languages, by Robert Atkey, Sam Lindley, and Jeremy Yallop](https://bentnib.org/unembedding.pdf)


