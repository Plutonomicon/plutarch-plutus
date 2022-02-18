> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

<details>
<summary> Table of Contents </summary>
- [Overview](#overview)
- [Untyped Plutus Core (UPLC)](#untyped-plutus-core--uplc-)
- [Plutarch Types](#plutarch-types)
- [Plutarch `Term`s](#plutarch--term-s)
  * [Plutarch Constant `Term`s](#plutarch-constant--term-s)
    + [Static building of constant `Term`s with `pconstant`](#static-building-of-constant--term-s-with--pconstant-)
    + [Dynamic building of constant `Term`s with `pcon`](#dynamic-building-of-constant--term-s-with--pcon-)
      - [Pattern matching `Term`s on `PType`s with `pmatch`.](#pattern-matching--term-s-on--ptype-s-with--pmatch-)
      - [`class (PCon a, PMatch a) => PlutusType (a :: PType)` -- with a default instance.](#-class--pcon-a--pmatch-a-----plutustype--a----ptype------with-a-default-instance)
    + [Overloaded literals](#overloaded-literals)
    + [Helper functions](#helper-functions)
  * [Lambdas; Plutarch-level Function `Term`s.](#lambdas--plutarch-level-function--term-s)
    + [Function Application](#function-application)
    + [Strictness and Laziness; Delayed Terms and Forcing](#strictness-and-laziness--delayed-terms-and-forcing)
- [References](#references)

</details>

# Overview

Plutarch is an eDSL in Haskell that can be used to write on-chain scripts for Cardano. With some caveats, Plutarch can be thought of as a [simply-typed lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus) (or STLC). We write scripts in Plutarch in order to leverage the language features provided by Haskell while retaining the ability to compile to compact Untyped Plutus Core (or UPLC, which itself is an untyped lambda calculus) scripts.

When we talk about "Plutarch scripts", we are referring to values of type `Term (s :: S) (a :: PType)`. `Term` is a `newtype` wrapper around a more complex type, the details of which Plutarch end-users can ignore. A `Term` is a typed lambda term; it can be thought of as representing a not-yet-evaluated computation that, if successfully evaluated, will return a value of type `a`. 

The two type variables of the `Term s a` declaration have particular kinds: 

- `s :: S` is like the `s` of `ST s a`. It represents the context of the computation in a way that mimics mutable state while still providing a functional interface. Sections 1 through 4 of [1] give an accessible introduction to how this works. `s` is never instantiated with a concrete value; it is merely a type-level way to ensure that computational contexts remain properly encapsulated (i.e., different state threads don't interact). For an explaination of how this is specifically used in eDSL design, see [2].
- `a :: PType` is short-hand for "Plutarch Type". These types are prefixed with a capital `P`, such as `PInteger`, `PBool`, and so forth. `PType` types are used to _tag_ terms with the type that they will produce. This allows us to bridge between the simple type system of Plutarch and the untyped UPLC -- carrying around the tags means that conversions to and from the `Data` representation required by UPLC terms and the simple type system of Plutarch `Term`s can be made more ergonomic. On the other hand, it also can provide an easier way to convert between Haskell values and Plutarch `Term`s via type synonyms.

Note that a type of kind `PType` _should not_ be thought of as carrying a value; it is a tag for a computation that may produce a value. For instance, the definition of `PInteger` is simply

```
data PInteger s
```

That is, there are no data constructors. _If_ a value of type `Term s PInteger` successfully executes the computation within context `s`, the value _computed_ will be an integer. We will never encounter values such as `y :: PInteger; y = 3`; they simply do not exist. While readers new to Plutarch may need some time to fit this into their mental model, it is an important distinction to keep in mind.

> For brevity, we will say a "value of type `Term s a` will evaluate to (...)". This phrase will carry two implicit notions: one, that `Term s a` represents a computation executed in the context `s`; two, that evaluating `Term s a` is not guarenteed to succeed.

In brief, when writing Plutarch scripts we have a few tasks:

- A.) Defining _Plutarch Types_ (or _`PType`s_), which are prefixed with a capital `P`, such as `PInteger`, `PMaybe a`, `PBool`, and so forth. As previously mentioned, these form the "tags" for Plutarch `Term`s, representing the type of result of compiling and evaluating a Plutarch Script.
- B.) Working with _Plutarch `Terms`_, which are values of the type `Term (s :: S) (a :: PType)`. These are the Plutarch scripts themselves, which can be compiled and executed. They can also be composed with other scripts.
- C.) Writing Haskell-level functions _between Plutarch Terms_ (i.e., with types like `Term s a -> Term s b`). Doing so allows us to leverage Haskell's language features and ecosystem.
- D.) Efficiently Converting the functions from _(C.)_ to _Plutarch-level functions_, which are of the type `Term s (a :--> b)`. At the most naive level, we can directly convert the functions from (C.) to Plutarch-level functions. Additional Plutarch utilities provide for optimization opportunities.
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

The remainder of this document cover the bridge between Haskell and Plutarch at a high-level. It will _not_ cover all techniques necessary to write production-ready scripts. Nor will it cover the bridge between Plutarch and UPLC beyond the minimum. Nonetheless, it should provide sufficient background to prepare the reader for further study.

# Untyped Plutus Core (UPLC)

Plutarch compiles to UPLC. This means that most Plutarch end-users will not need to concern themselves with the details of UPLC, but a brief overview will aid in building a mental model of how Plutarch works.

Unlike Haskell, UPLC is a low-level and untyped language implementing a basic lambda calculus. It supports only a handful of builtin values and functions that are then strung together in lambda applications. The builtin types provided by UPLC include the usual primitive types -- integers, bytestrings and strings, booleans, and so forth -- as well as a special `Data` value which provides the machinery necessary to encode untyped representations of arbitrary sum-of-products Haskell types.

While the _semantic_ meaning of a Haskell type such as `Maybe Integer` is missing in UPLC, it still can be _represented_ as `Data` in UPLC -- UPLC simply doesn't keep keep track of what differentiates semantically distinct `Data` values, and will not prevent a programmer from operating on the `Data` representation in non-sensical ways. 

The solution Plutarch provides is to _tag_ scripts (of the type `Term s a`) that compile to UPLC with types. This means that the various operations on semantically distinct `Data` values can be tracked and typed check by the Plutarch compiler. These tags are provided by "Plutarch Types", or "types of kind `PType`". 

In order for the Plutarch compiler to bridge between arbitrary, semantically-rich Haskell types and the untyped `Data` of UPLC, it is obviously necessary to associate various bits of information with `PType`s. On the one hand, each `PType` should have some semantic, type-level richness to it such as typeclass instances (otherwise there would be little point in programming in Haskell!). On the other hand, each `PType` needs to have a UPLC represenation, either as a builtin primitive value or as `Data`, so that we can actually compile to UPLC.

# Plutarch Types

When this guide uses the term "Plutarch Type", we are specifically talking about a type of _kind_ `PType`. Types of kind `PType` are prefixed with a capital `P`, such as `PBool` (and this convention should be adopted when defining new `PTypes`.) We will refer to  _"types of kind `PType`"_ simply as "`PType`s". If we need to refer to the _kind_ `PType`, we explicitly qualify this.

`PType` is defined as `type PType = S -> Type`; that is, it is a _kind synonym_ for `S -> Type` (where `S` and `Type` are themselves kinds). This is important to keep in mind, because when querying the kind of something like `PBool` in, say, GHCi, we will _not_ see `PType` as the kind:

```haskell
ghci> :k PBool
PBool :: S -> Type
```

Thus, any time we see the kind `S -> Type`, we should mentally substitute its kind synonym `PType`. Types of kind `PType`, should be thought of as _tags_ on computation. They do not represent types of values in the same way as standard Haskell types. 

The kind of basic types such as `Integer` in Haskell is `*`; the corresponding "basic" kind in Plutarch is simply `PType`. Higher-kinded types in Haskell, such as `Maybe`, will kinds such as `* -> *`. In Plutarch, the corresponding kind is:

```haskell
ghci> :k PMaybe
PMaybe :: PType -> S -> Type
```

Since the kind arrow `->` is right associative, this can be read as `PMaybe :: PType -> (S -> Type)`; and since we know that that `PType` and `S -> Type` and synonyms, we see this as `PMaybe :: PType -> PType`, which should agree with out intuition.

The kind `S -> Type` is mysterious at first, but we recall from the introduction that `PType`s are _tags_ on (unexecuted) computations to indicate the type of their result. The `S` kind represents the computational context; thus, a `PType` expects to recieve a _computational context_ represented by a value `s` whose type has kind `S` that it will tag to produce a `Type`. Note that the kind `S` and the value `s` are never instantiated by end-users with concrete values; they are simply type-level mechanisms to ensure that functional purity is maintained.

The above notion is important to understanding why not all `PType`s have data constructors; the data constructors are irrelevant, except insofar as they enable the implementation to keep track of Haskell-level and UPLC-level representations. `PInteger` is one such case; it is impossible to construct a constant `y` where `y = PInteger`. Other `PType`s, such as `PMaybe`, _do_ have data constructors (specifically `PJust` and `PNothing`), but _still_ do not carry data from the viewpoint of UPLC. A value such as `PNothing` represents to the compiler that the `Data` representation of the result of an executed UPLC script should be interpreted as `PNothing` (bridging UPLC with Plutarch), and should typecheck and carry the same semantics as Haskell's `Nothing`; nothing more and nothing less is implied. 

# Plutarch `Term`s

Plutarch `Term`s are terms in the sense of simply-typed lambda calculus terms. This means that we can construct terms as either "constants" or "lambdas", and terms can either be "open" (having free variables) or "closed" (having no free variables). Plutarch `Term`s are used to build up increasingly complex computations. Once all free variables are eliminated from a `Term` (making it a `Closed Term`) it can be compiled using the eponymous function from the `Plutarch` module:

```haskell
-- | Closed term is a type synonym 
type ClosedTerm (a :: PType) = forall (s :: S). Term s a 

-- | Compile operates on closed terms to produce usable UPLC scripts.
compile :: ClosedTerm a -> Script
```
`Term`s are constructed from Haskell values and are tagged with `PType`s.

## Plutarch Constant `Term`s

A constant Plutarch `Term` is one that will always yield the same result when evaluated. There are several ways of building constant `Term`s:

- Statically buiilding constant `Term`s from concrete Haskell values , when the value is known at compile-time. 
- Dynamically building constant `Term`s from Haskell values, i.e. when the the constant produced depends on a function argument
- Overloaded literal syntax
- Helper functions
  
### Static building of constant `Term`s with `pconstant`

If the desired value of a constant `Term` is known at compile time, we can build the `Term` directly using [Haskell synonyms](./CONCEPTS.md#haskell-synonym-of-plutarch-types). The function to do so is `pconstant`. 

Constructing constants in this way utilizes [associated type familes](https://wiki.haskell.org/GHC/Type_families#An_associated_type_synonym_example) provided in the `PConstant` and `PLift` typeclasses. One the Haskell <-> Plutarch side, the basic flow is this:

- `pconstant` takes a single argument. The type of That argument must be an instance of the `PUnsafeLiftDecl` typeclass.
- The `PUnsafeLiftDecl` has a superclass, `PConstant`. This class (among other things) associates a type family `PConstanted h :: PType` to a Haskell type `h :: Type`.
- This means that `pconstant` can take a value `x` of type `Type` and produce a type of kind `PType`. This allows `pconstant` to embed the value `x` into a `Term s a`, where `a` is the `PType` associated with the type of `x`. 
- The computation represented by `Term s a` is now tagged with the appropriate type.

On the Plutarch <-> UPLC side, the flow is as follows:

- The `PConstant` class associates a second type family, `PConstantRepr`, with the Haskell type `h :: Type`. This type family gives the UPLC representation of `h`. 
- When a `Term` is constructed as above, the compilation of that term is guided to the on-chain UPLC representation by this type family.

The end-user effect `pconstant` always takes in a regular Haskell value with a `PLift`, and thus `PConstant` typeclass instances (along with the other superclasses of each) to create its Plutarch synonym. These typeclasses may seem burdensome, but they are provided by the Plutarch libraries for most basic types and can be automatically derived in many other cases. For example:

```hs
import Plutarch.Prelude

-- | A plutarch level boolean. Its value is "True", in this case.
x :: Term s PBool
x = pconstant True
```

To reiterate: in the above snippet, `x` represents a computation that is _tagged_ with the type `PBool`, that, when evaluted, returns a result that should be interpreted as `True`. The `PLift` and `PConstant` typeclasses provided the associated types necessary for Plutarch to convert to and from Haskell types to Plutarch's `PType` tags. 

### Dynamic building of constant `Term`s with `pcon`

Sometimes the value that we want to treat as a constant `Term` is not known at compile time. To explain how to construct constants when the value can only be determined at runtime, we will examine the `PMaybe` Plutarch type. It can serve the same purpose as the `Maybe` type in Haskell: to represent the situation where a computation may not produce a sensible result. 

`PMaybe` has the following definition:

```
data PMaybe (a :: PType) (s :: S)
  = PJust (Term s a)
  | PNothing
```
and the following kind:
```
ghci> :k PMaybe
PMaybe :: PType -> S -> Type
```

Let's dissect what this means.

- `PMaybe` builds a `PType` from a `PType`. This means that, given a `PType`, we can tag a computation with the type `PMaybe a` to indicate that its return value should be interpreted as either `Just a` or `Nothing`. Such a tagging would look like a value with the type `Term s (PMaybe a)`.
- `PJust` and `PNothing` are data constructors. They themselves are _not_ tags. `PJust :: Term s a -> PMaybe (a :: PType) (s :: S)` is a _wrapper_ around a computation that can return a result of type `a`. 

Now suppose that we want to carry around a constant `Term` in a Plutarch script that can be either `PJust a` or `PNothing`. To do so, we need a way go from `PJust a` (which, unlike `PInteger`, _can_ be instantiated) to a `Term s (PMaybe a)`. This what `pcon` is for: 

```
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
- The type of `justx` says the following: "this type represents a tag that can be applied to a computation to indicate that the computation, evaluated in the context `s`, will either produce a `Just Integer` or `Nothing`".
- The type of `justxTerm` represents a computation that is tagged with the type of `justx`.

That is, if we ask `justxTerm` what it will return when evaluted, it responds "I will give you a value that should be interpreted as either `Nothing` or `Just Integer`". Of course, here we know that the result will always be `Just 3`; but this is the general mechanism to declare a function requiring a `Maybe`. We will revisit this example when we discusses how to construct [Lambdas](#lambdas).

The `pcon` function is a method of the [`PCon` typeclass](./TYPECLASSES.md#plutustype-pcon-and-pmatch).
    
#### Pattern matching `Term`s on `PType`s with `pmatch`.

Given that we've show how to construct `Term`s out of the data constructors of types with kind `PType` (i.e., `pcon . PJust`), it is natural that we may want to pattern match on `Term` with a known `PType` tag (i.e., of a value with type `Term s (PMaybe a)`) to produce another `Term` (i.e., depending on whether the value matches `PJust _` or `Nothing`.)

The function that we need is a method of the `PMatch` typeclass. For the time being, we will ignore the details of implementation and only look at the type:

```haskell
pmatch :: forall (a :: PType) (s :: S) (b :: PType).
    PMatch a =>               {- We have two constraints on the type `a`: 
                                  it has a `PMatch` instance, and is of kind `PType`.-}
    Term s a ->               -- Given a `Term` tagged with `a`...
    (a s -> Term s b) ->      -- ...and a function from a type of kind `PType` to a Term s b)`...
    Term s b                  -- ...produce a `Term s b`
```

The annotation of the second argument deserves some focus; the second argument has it's type displayed as `(a s -> Term s b)`, but our comment suggests something else. This is because `a` is declared to have kind `PType`, and `PType` is a kind synonym for `S -> Type`. Thus, since `s` is declared to have kind `S`, we have that `a s` has the _kind_ `PType`.

What this means, in practice, is that `pmatch` matches on the possible values of the _result_ of evaluating a `Term s a` -- specifically, it matches on _values_ of a _type_ that have _kind `PType`_ -- and branches accordingly. The second argument to `pmatch` is called a _contination_; it determines how the program continues once `pmatch` has done its work.

We have already introduced a type with kind `PType` that is suitable for branching. Here is an example of a :

```
{- | This function takes in a Haskell-level `PMaybe` value (specifically, _not_ a `Term`)
     and return a `Term` depending on the Haskell-level pattern match on `PMaybe`s data
     constructors.
-}
continuation :: forall {a :: PType} {s :: S}. PMaybe a s -> Term s PBool
continuation x = case x of
   PJust _ -> pconstant True
   PNothing -> pconstant False

{- | A Haskell-level `isJust` on Plutarch `Term`s. `pmatch` is able to match on
     the possibilities of `PJust _` or `PNothing` being the result of an evaluated
     `Term`.
-}
hisJust :: forall {a :: PType} {s :: S}. Term s (PMaybe a) -> Term s PBool
hisJust x = pmatch x continuation

-- | A Plutarch-level `isJust`
pisJust :: forall {s :: S} {a :: PType}. Term s (PMaybe a :--> PBool)
pisJust = plam hisJust
```
Readers should note that this is not the most ergonomic way to deal with pattern matching (two versions of `do` syntax are provided), but it _is_ how the more ergonomic methods work under the hood.

#### `class (PCon a, PMatch a) => PlutusType (a :: PType)` -- with a default instance.
The combination of the `PCon` and `PMatch` typeclasses (providing `pcon` and `pmatch`, respectively) is sufficient to encode Haskell types as UPLC types. 
Having both a `PCon` and `PMatch` instance means that the `PlutusType` instance -- the instance detailing how to go from Plutarch `Term`s to UPLC representations -- can be derived generically!

While we won't go into detail here, this is a non-trivial fact that deserves explicit mention; we've gone from Haskell types to Plutarch `PType` tags, to tagged Plutarch `Terms` and back, and obtained a mechanism to represent UPLC terms for free.

### Overloaded literals

`pconstant` and `pcon` are the long-form ways of building constants. Certain constant Haskell literals are overloaded to help build Plutarch constants. Two examples are provided below.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Plutarch.Prelude

-- | A plutarch level integer. Its value is 1, in this case.
x :: Term s PInteger
x = 1

-- | A plutarch level string (this is actually 'Text'). Its value is "foobar", in this case.
y :: Term s PString
y = "foobar"
```

### Helper functions

Finally, other miscellaneous functions provided by Plutarch can be used to build constants:

```haskell
import qualified Data.ByteString as BS
import Plutarch.Prelude

-- | A plutarch level bytestring. Its value is [65], in this case.
x :: Term s PByteString
x = phexByteStr "41"
-- ^ 'phexByteStr' interprets a hex string as a bytestring. 0x41 is 65 - of course.
```

## Lambdas; Plutarch-level Function `Term`s.

Lambdas are the second form of Plutarch `Term`s. Lambda terms are represented at the type level by the infix type constructor `:-->`; a value of type `Term s (a :--> b)` evaluates to a function that takes a value of type `a` and produces a value of type `b`.

You can create Plutarch lambda `Term`s by applying the `plam` function to a Haskell-level function that works on Plutarch terms. The true type of `plam` itself is unimportant to end-users of Plutarch, but it should be thought of as 

```haskell
plam :: (Term s a -> Term s b) -> Term s (a :--> b)
```

To create the identity function as a Plutarch lambda, we would thus use:

```haskell

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

Notice the type. A Plutarch lambda `Term` uses the `:-->` infix operator to encode a function type. In the above case, `pid` is a Plutarch level function that takes a type `a`, and returns the same type. As one would expect, `:-->` is right associative and things curry like a charm.

Guess what this Plutarch level function does:

```haskell
f :: Term s (PInteger :--> PString :--> a :--> a)
```

It takes in an integer, a string, and a type `a` and returns the same type `a`. Notice that the types are all of kind `PType`. This means that when faced with filling out the gap:

```haskell
f :: Term s (PInteger :--> PString :--> a :--> a)
f = plam $ \???
```

We know that the argument to `plam` here is a Haskell function `g` with type `Term s PInteger -> Term s PString -> Term s a -> Term s a`.

### Function Application

Once a Plutarch lambda `Term` has been constructed using `plam`, it is rather useless unless it can be applied to an argument. Plutarch provides two operators to do so 

```
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
  '$', in combination with '#'. e.g.:

  >>> f # x #$ g # y # z
  f x (g y z)
-}
(#$) :: Term s (a :--> b) -> Term s a -> Term s b
infixr 0 #$
```

The types of each match our intuition; applying a lambda `Term` to a `Term` (tagged with the `PType` of the domain of the lambda) produced a `Term` (tagged with the `PType` of the codomain.).

### Strictness and Laziness; Delayed Terms and Forcing

Plutarch, like UPLC, is strict by default; this is in contrast to Haskell, which is nonstrict. What this means, in practice, is that _all_ arguments to a Plutarch lambda `Term` are evaluated before the function is called. 

> Note: the below example does not correspond exactly to the implementation of `pif` or `pif'`; it is for didactic purposes only

An example of where this may be undesirable is where one of two `Term`s are branched upon in an `if` statement. In Plutarch, `pif'` is strict and `pif` is lazy. A strict `pif'` could look something like 

```
{- | Haskell-level `if` function between `Term`s. This is nonstrict;
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

This is undesirable for the obvious reason: only one branch will be selected at runtime, so it doesn't make sense to evaluate both prior to examining the `PBool` value to which we apply `pif`.

In order to avoice this, You can use `pdelay` to create a "delayed `Term`". This wraps the `PType` tag of a term to indicate that the term should _not_ be evaluated immediately. It has the following type:

```
pdelay :: Term s a -> Term s (PDelayed a)
```

A delayed term is evaluated when _forced_ using the `pforce` function. Forcing a term simply strips the `PDelayed` wrapper:

```
pforce :: Term s (PDelayed a) -> Term s a 
```

Thus, if we wanted a lazy `pif'`, we could do the following:

```
{- | Haskell-level `if` function between `Term's`. This time
it _does_ translate to a lazy Plutarch lambda.
-}
hif :: Term s PBool -> Term s a -> Term s a -> Term s a
hif cond whenTrue whenFalse = pforce $ pif' # cond # pdelay whenTrue # pdelay whenFalse

{- | Plutarch-level `if`. The arguments in `hif` are delayed, so they are not applied
until they are forced.
-}
pif :: forall {s :: S} {b :: PType}. Term s (PBool :--> (b :--> (b :--> b)))
pif = plam pif'
```

A note of caution: calling `pforce` on the same delayed term twice will execute the computation each time. Users familiar with Haskell's handling of laziness -- where forcing a thunk twice never duplicates computation -- should note that this UPLC behaves differently.

Finally, readers should note that `pdelay` and `pforce` are extremely powerful tools when writing Plutarch scripts, and are encouraged to familiarize themselves accordingly. 


# References 

- [1] [Lazy Functional State Threads, by John Launchbury and Simon L Peyton Jones](https://www.microsoft.com/en-us/research/wp-content/uploads/1994/06/lazy-functional-state-threads.pdf) 
- [2] [Unembedding Domain-Specific Languages, by Robert Atkey, Sam Lindley, and Jeremy Yallop](https://bentnib.org/unembedding.pdf)


