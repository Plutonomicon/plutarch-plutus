This document describes various concepts applicable in Plutarch.

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

<details>
<summary> Table of Contents </summary>

- [Hoisting, metaprogramming, and fundamentals](#hoisting--metaprogramming--and-fundamentals)
  * [Hoisting Operators](#hoisting-operators)
- [What is the `s`?](#what-is-the--s--)
- [Data encoding and Scott encoding](#data-encoding-and-scott-encoding)
  * [Data encoding](#data-encoding)
  * [Scott encoding](#scott-encoding)
- [Haskell synonym of Plutarch types](#haskell-synonym-of-plutarch-types)



</details>

# Hoisting, metaprogramming, and fundamentals

What is essentially happening here, is that we have a two-stage compilation process.

First GHC compiles our code, then our code generates an _AST_ of our Plutus script,

which is then serialized using `compile`.

The important thing to note, is that when you have a definition like:

```haskell
x :: Term s PInteger
x = something complex
```

Any use of `x` will inline the **full definition** of `x`. `x + x` will duplicate `something complex` in the AST. To avoid this, you should [use `plet` in order to avoid duplicate work](#plet-to-avoid-work-duplication). Do note that this is **strictly evaluated, and hence isn't always the best solution.**

There is however still a problem: what about top-level functions like `fib`, `sum`, `filter`, and such? We can use `plet` to avoid duplicating the definition, but this is error-prone, since to do this perfectly each function that generates part of the AST would need to have access to the `plet`'ed definitions, meaning that we'd likely have to put it into a record or typeclass.

To solve this problem, Plutarch supports _hoisting_. Hoisting only works for _closed terms_, that is, terms that don't reference any free variables (introduced by `plam`).

Hoisted terms are essentially moved to a top-level `plet`, i.e. it's essentially common sub-expression elimination. Do note that because of this, your hoisted term is **also strictly evaluated**, meaning that you _shouldn't_ hoist non-lazy complex computations (use [`pdelay`](./CONCEPTS.md#delay-and-force) to avoid this).

## Hoisting Operators

For the sake of convenience, you often would want to use operators - which must be Haskell level functions. This is the case for `+`, `-`, `#==` and many more.

Choosing convenience over efficiency is difficult, but if you notice that your operator uses complex logic and may end up creating big terms - you can trivially factor out the logic into a Plutarch level function, hoist it, and simply apply that function within the operator.

Consider "boolean or":

```hs
(#||) :: Term s PBool -> Term s PBool -> Term s PBool
x #|| y = pif x (pconstant True) $ pif y (pconstant True) $ pconstant False
```

You can factor out most of the logic to a Plutarch level function, and apply that in the operator definition:

```hs
(#||) :: Term s PBool -> Term s PBool -> Term s PBool
x #|| y = por # x # pdelay y

por :: Term s (PBool :--> PDelayed PBool :--> PBool)
por = phoistAcyclic $ plam $ \x y -> pif' # x # pconstant True # pforce y
```

In general the pattern goes like this:

```hs
(<//>) :: Term s x -> Term s y -> Term s z
x <//> y = f # x # y

f :: Term s (x :--> y :--> z)
f = phoistAcyclic $ plam $ \x y -> <complex computation>
```

(OR, simply inlined)

```hs
(<//>) :: Term s x -> Term s y -> Term s z
x <//> y = (\f -> f # x # y) $ phoistAcyclic $ plam $ \x y -> <complex computation>
```

> Note: You don't even need to export the Plutarch level function or anything! You can simply have that complex logic factored out into a _hoisted, internal Plutarch function_ and everything will work just fine!

# What is the `s`?

The `s` essentially represents the context, and is like the `s` of `ST`.

It's used to distinguish between closed and open terms:

- Closed term: `type ClosedTerm = forall s. Term s a`
- Arbitrary term: `exists s. Term s a`
- NB: `(exists s. Term s a) -> b` is isomorphic to
- `forall s. Term s a -> b`




# Data encoding and Scott encoding

In Plutus Core, there are really two (conflicting) ways to represent non-trivial ADTs- [`Constr`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html#t:Data) data encoding, or Scott encoding. You can (you should!) only use one of these representations for your non-trivial types.

> Aside: What's a "trivial" type? The non-data builtin types! `PInteger`, `PByteString`, `PBuiltinList`, `PBuiltinPair`, and `PMap` (actually just a builtin list of builtin pairs). It's important to note that [`Data`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html#t:Data) (`Constr` or otherwise) is also a builtin type.

## Data encoding

`Constr` data is essentially a sum-of-products representation. However, it can only contain other `Data` values (not necessarily just `Constr` data, could be `I` data, `B` data etc.) as its fields. Plutus Core famously lacks the ability to represent functions using this encoding, and thus - `Constr` encoded values simply cannot contain functions.

> Note: You can find out more about the deep details of `Data`/`BuiltinData` at [plutonomicon](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md).

With that said, `Data` encoding is _ubiquitous_ on the chain. It's the encoding used by the ledger api types, it's the type of the arguments that can be passed to a script on the chain etc. As a result, your datums and redeemers _must_ use data encoding.

## Scott encoding

On the opposite (and conflicting) end, is Scott encoding. [The internet](https://crypto.stanford.edu/~blynn/compiler/scott.html) can explain Scott encoding way better than I can. But I'll be demonstrating Scott encoding with an example anyway.

Firstly, what good is Scott encoding? Well it doesn't share the limitation of not being able to contain functions! However, you cannot use Scott encoded types within, for example, your datums and redeemers.

Briefly, Scott encoding is a way to represent data with functions. The Scott encoded representation of `Maybe a` would be:

```hs
(a -> b) -> b -> b
```

`Just 42`, for example, would be represented as this function:

```hs
\f _ -> f 42
```

Whereas `Nothing` would be represented as this function:

```hs
\_ n -> n
```

We covered construction. What about usage/deconstruction? That's also just as simple. Let's say you have a function, `foo :: Maybe Integer -> Integer`, it takes in a Scott encoded `Maybe Integer`, and adds `42` to its `Just` value. If it's `Nothing`, it just returns `0`.

```hs
{-# LANGUAGE RankNTypes #-}

import Prelude (Integer, (+))

type Maybe a = forall b. (a -> b) -> b -> b

just :: a -> Maybe a
just x = \f _ -> f x

nothing :: Maybe a
nothing = \_ n -> n

foo :: Maybe Integer -> Integer
foo mb = mb (\x -> x + 42) 0
```

How does that work? Recall that `mb` is really just a function. Here's how the application of `f` would work:

```hs
foo (just 1)
foo (\f _ -> f 1)
(\f _ -> f 1) (\x -> x + 42) 0
(\x -> x + 42) 1
43
```

```hs
foo nothing
foo (\_ n -> n)
(\_ n -> n) (\x -> x + 42) 0
0
```

Neat!

This is the same recipe followed in the implementation of `PMaybe`. See its [PlutusType impl](./TYPECLASSES.md#plutustype-pcon-and-pmatch)!

# Haskell synonym of Plutarch types
Several sections of the guide use the terminology "Haskell synonym". What does it mean? It's simply the Haskell type that _is supposed to_ correspond to a Plutarch type. There doesn't _necessarily_ have to be some sort of concrete connection (though there can be, using [`PLift`/`PConstant`](./TYPECLASSES.md#pconstant--plift)) - it's merely a connection you can establish mentally.

This detail does come into play in concrete use cases though. After compiling your Plutarch code to a `Script`, when you pass Haskell data types as arguments to the `Script` - they obviously need to correspond to the actual arguments of the Plutarch code. For example, if the Plutarch code is a function taking `PByteString`, after compilation to `Script`, you _should_ pass in the Haskell data type that actually shares the same representation as `PByteString` - the "Haskell synonym", so to speak. In this case, that's `ByteString`\*.

\[\*]: You can't actually pass a `ByteString` into a compiled `Script` through the [`Plutus.V1.Ledger.Scripts`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Scripts.html) API. Notice that you can only pass `Data` arguments using [`applyArguments`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Scripts.html#v:applyArguments). The Haskell synonym to `Data` is `PAsData a` (for any `a`), and `PData`.

Also see: [Figuring out the representation of a Plutarch type](./TRICKS.md#figuring-out-the-representation-of-a-plutarch-type).

