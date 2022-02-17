This document describes various concepts applicable in Plutarch.

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

<details>
<summary> Table of Contents </summary>

- [Hoisting, metaprogramming, and fundamentals](#hoisting-metaprogramming-and-fundamentals)
  - [Hoisting Operators](#hoisting-operators)
- [What is the `s`?](#what-is-the-s)
- [eDSL Types in Plutarch](#edsl-types-in-plutarch)
- [`plet` to avoid work duplication](#plet-to-avoid-work-duplication)
- [Tracing](#tracing)
- [Raising errors](#raising-errors)
- [Delay and Force](#delay-and-force)
- [Data encoding and Scott encoding](#data-encoding-and-scott-encoding)
  - [Data encoding](#data-encoding)
  - [Scott encoding](#scott-encoding)
- [Haskell synonym of Plutarch types](#haskell-synonym-of-plutarch-types)
- [Unsafe functions](#unsafe-functions)

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

# eDSL Types in Plutarch

Most types prefixed with `P` are eDSL-level types, meaning that they're meant to be used with `Term`. Their kind must be `PType`.

They are merely used as a tag, and what Haskell value they can hold is not important. This is an eDSL after all! Their data declarations _actually_ don't matter as far as internal semantics are concerned.

The only thing concrete here is the runtime representation of the type: what it looks like in Plutus Core. Usually this connection is established in the Plutarch type's `PlutusType` implementation\*. You see, `PlutusType` helps *connect* an essentially transient, semantically meaningless tag, with a real runtime value. The tag then helps with typing. It's a *typed* eDSL!

> \[\*]: Not all type's representations are established through `PlutusType`, but it's the most prominent one for our user facing interface. The truth is, anything that can build a `Term s P` from scratch, for some Plutarch type `P`, is capable of *determining* the representation of `P`. But for type safety, we have to constrain it so that several different functions can't build a `Term s P` and just choose their own representation for it. That breaks everything!

As such, don't focus much into what value a Plutarch type contains in the Haskell world - it doesn't matter. Even the constructors and fields are only there to help with `PlutusType` implementation, by further connecting all of it with runtime values.

For example, `PInteger` doesn't hold *any* values. There is no constructor, it's simply an empty data declaration. `PInteger` is interesting as its representation is determined by the `Num (Term s PInteger)` instance, not `PlutusType`. How?! Well, `fromInteger` is a rather convenient function: it lets you build "number types"  from integers. So `fromInteger` simply takes an integer, we do some magic to create a Plutus Core integer constant out of it, and declare the return type to be `Term s PInteger`. Boom! We built a term of type `PInteger` ....and yet, we never even cared about the actual `PInteger` data declaration.

Let's get to a more orthodox example: `PMaybe`. We discuss its `PlutusType` instance in [the relevant section](./TYPECLASSES.md#plutustype-pcon-and-pmatch). Here's the data type declaration-
```hs
data PMaybe a s = PJust (Term s a) | PNothing
```
`pcon PNothing` simply yields a runtime value that is capable of representing the *concept of `Nothing`*. It never actually uses `PNothing` - it's just there to let `pcon` know: "hey, I want to represent `Nothing`".

The case is similar for `pcon (PJust x)` (`x :: Term s a`). Notice that `x` *is* a runtime concept with semantic meaning. It's a Plutarch term - it's as concrete as it gets. However, that `PJust` constructor is simply thrown away to obtain the actual term. The term is then put into a runtime value that is capable of representing the *concept of `Just`*. We have used Scott encoding to represent the concept of `Just` and `Nothing` in Plutus Core, for this specific case. But it could have been something else!

In all of this, we've ignored a major Plutarch term building concept: `pconstant`. After all, `pconstant` *also* builds terms for Plutarch types - doesn't that mean it also has a say in connecting the representation with the transient tag? Yes, yes it does. The details of how everything fits in is irrelevant for the user facing interface. But just know that as long as you use the safe derivers to implement these typeclasses - you should be fine, the several "runtime representation determiner"s will merrily agree with each other and the type system (that is, Plutarch's own type system) will be sound.

Also see: [Figuring out the representation of a Plutarch type](./TRICKS.md#figuring-out-the-representation-of-a-plutarch-type).

# `plet` to avoid work duplication

Sometimes, when writing Haskell level functions working on Plutarch terms, you may find yourself needing to re-use the Haskell level function's argument(s) multiple times:

```hs
foo :: Term s PString -> Term s PString
foo x = x <> x
```

In such cases, you should use `plet` on the argument to [avoid duplicating work](./TRICKS.md#dont-duplicate-work).

# Tracing

You can use the functions `ptrace`, `ptraceError`, `ptraceIfFalse`, `ptraceIfTrue` (from `Plutarch.Trace` or `Plutarch.Prelude`) for tracing. These behave similarly to the ones you're used to from [PlutusTx](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Trace.html).

If you have the `development` flag for `plutarch` turned on - you'll see the trace messages appear in the trace log during script evaluation. When not in development mode - these functions basically do nothing.

# Raising errors

In PlutusTx, you'd signal validation failure with the [`error`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Prelude.html#v:error) function. You can do the same in Plutarch using `perror`.

```hs
fails :: Term s (PData :--> PData :--> PData :--> PUnit)
fails = plam $ \_ _ _ -> perror
```

# Delay and Force

Use `pdelay` on a term to create a "delayed term".

```hs
let f = plam (\x -> x) in pdelay (f # phexByteStr 0x41)
```

Compiling and evaluating it yields:

    Program () (Version () 1 0 0) (Delay () (Constant () (Some (ValueOf bytestring "A"))))

The function application is "delayed". It will not be evaluated (and therefore computed) until it is _forced_.

Plutarch level function application is strict. All of your function arguments are evaluated **before** the function is called. This is often undesirable, and you want to create a delayed term instead that you want to force _only_ when you need to compute it.

You can force a previously delayed expression using `pforce`:

```hs
pforce $ let f = plam (\x -> x) in pdelay (f # phexByteStr "41")
```

It evaluates to:

    Program () (Version () 1 0 0) (Constant () (Some (ValueOf bytestring "A")))

Delaying the argument to a Plutarch level function, within the function body, is not very useful - since the argument has been evaluated before the function body has been entered! Instead, you'll often notice usage of `pdelay` and `pforce` in Haskell level function arguments to simulate laziness:

```hs
pif' :: Term s (PBool :--> a :--> a :--> a)

-- | Lazy if-then-else.
pif :: Term s PBool -> Term s a -> Term s a -> Term s a
pif cond whenTrue whenFalse = pforce $ pif' # cond # pdelay whenTrue # pdelay whenFalse
```

`pif'` is a direct synonym to the `IfThenElse` Plutus Core builtin function. Of course, it evaluates its arguments strictly but you often want an if-then-else that doesn't evaluate both its branches - only the one for which the condition holds. So, `pif`, as a Haskell level function can take in both branches (without any concept of evaluating them), delay them and _then_ apply them to `pif'`. Finally, a `pforce` will force the yielded branch that was previously delayed.

> Aside: Be careful of `pforce`ing the same delayed term twice. Unlike Haskell's handling of laziness - where forcing a thunk twice never duplicates computation - UPLC (Untyped Plutus Core) will happily duplicate the computation each time you force it.

Delay and Force will be one of your most useful tools while writing Plutarch. Make sure you get a grip on them!

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

# Unsafe functions

There are internal functions such as `punsafeCoerce`, `punsafeConstant` etc. that give you terms without their specific type. These **should not** be used by Plutarch users. It is the duty of the user of these unsafe functions to get the type right - and it is very easy to get the type wrong. You can easily make the type system believe you're creating a `Term s PInteger`, when in reality, you created a function.

Things will go very wrong during script evaluation if you do that kind of thing.

The good thing is that unsafe functions all have explicit indicators through the names, as long as you don't use any `punsafe*` functions - you should be fine!

Of course, these have legitimate use cases. Most often, we use these functions to convert between types that _truly_ have the same internal representation in UPLC - but the type system simply isn't expressive enough to infer that.
