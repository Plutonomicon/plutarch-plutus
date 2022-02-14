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
- [Unsafe functions](#unsafe-functions)

</details>

# Hoisting, metaprogramming, and fundamentals

What is essentially happening here, is that we have a 2-stage compilation process.

First GHC compiles our code, then our code generates an _AST_ of our Plutus script,

which is then serialized using `compile`.

The important thing to note, is that when you have a definition like:

```haskell
x :: Term s PInteger
x = something complex
```

Any use of `x` will inline the **full definition** of `x`. `x + x` will duplicate `something complex` in the AST. To avoid this, you should [use `plet` in order to avoid duplicate work](#plet-to-avoid-work-duplication). Do note that this is **strictly evaluated, and hence isn't always the best solution.**

There is however still a problem: What about top-level functions, like `fib`, `sum`, `filter`, and such? We can use `plet` to avoid duplicating the definition, but this is error-prone, since to do this perfectly each function that generates part of the AST would need to have access to the `plet`'ed definitions, meaning that we'd likely have to put it into a record or typeclass.

To solve this problem, Plutarch supports _hoisting_. Hoisting only works for _closed terms_, that is, terms that don't reference any free variables (introduced by `plam`).

Hoisted terms are essentially moved to a top-level `plet`, i.e. it's essentially common subexpression elimination. Do note that because of this, your hoisted term is **also strictly evaluated, meaning that you shouldn't hoist non-lazy complex computations (use e.g.** `pdelay` **to avoid this).**

## Hoisting Operators
For the sake of convenience, you often would want to use operators - which must be Haskell level functions. This is the case for `+`, `-`, `#==` and many more.

Choosing convenience over efficiency is difficult, but if you notice that your operator uses complex logic and may end up creating big terms - you can trivially factor out the logic into a Plutarch level function, hoist it, and simply apply that function within the operator.

Consider boolean or-
```hs
(#||) :: Term s PBool -> Term s PBool -> Term s PBool
x #|| y = pif x (pconstant True) $ pif y (pconstant True) $ pconstant False
```
You can factor out most of the logic to a Plutarch level function, and apply that in the operator definition-

```hs
(#||) :: Term s PBool -> Term s PBool -> Term s PBool
x #|| y = por # x # pdelay y

por :: Term s (PBool :--> PDelayed PBool :--> PBool)
por = phoistAcyclic $ plam $ \x y -> pif' # x # pconstant True # pforce y
```

In general the pattern goes like this-
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

> Note: You don't even need to export the Plutarch level function or anything! You can simply have that complex logic factored out into a *hoisted, internal Plutarch function* and everything will work just fine!

# What is the `s`?

The `s` essentially represents the context, and is like the `s` of `ST`.

It's used to distinguish between closed and open terms:

- Closed term: `type ClosedTerm = forall s. Term s a`
- Arbitrary term: `exists s. Term s a`
- NB: `(exists s. Term s a) -> b` is isomorphic to
- `forall s. Term s a → b`

# eDSL Types in Plutarch

Most types prefixed with `P` are eDSL-level types, meaning that they're meant to be used with `Term`. They are merely used as a tag, and what Haskell value they can hold is not important. Their kind must be `PType`.

# `plet` to avoid work duplication
Sometimes, when writing Haskell level functions for generating Plutarch terms, you may find yourself needing to re-use the Haskell level function's argument multiple times-
```hs
foo :: Term s PString -> Term s PString
foo x = x <> x
```
This is *really* bad if `x` is actually represented by a big unevaluated computation yielding `Term s PString`. Whenever you find yourself using a Haskell level function argument multiple times - you may want to *strictly* evaluate it first. `plet` lets you do just that-
```hs
foo :: Term s PString -> Term s PString
foo x' = plet x' $ \x -> x <> x
```

Also see: [Don't duplicate work](#dont-duplicate-work).

# Tracing
You can use the functions `ptrace`, `ptraceError`, `ptraceIfFalse`, `ptraceIfTrue` (from `Plutarch.Trace`) for tracing. These behave similarly to the ones you're used to from [PlutusTx](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Trace.html).

If you have the `development` flag for `plutarch` turned on - you'll see the trace messages appear in the trace log during script evaluation. When not in development mode - these functions basically do nothing.

# Raising errors
In Plutus Tx, you'd signal validation failure with the [`error`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Prelude.html#v:error) function. You can do the same in Plutarch using `perror`.
```hs
fails :: Term s (PData :--> PData :--> PData :--> PUnit)
fails = plam $ \_ _ _ -> perror
```

# Delay and Force
Use `pdelay` on a term to create a "delayed term".
```hs
let f = plam (\x -> x) in pdelay (f # phexByteStr 0x41)
```
Compiling and evaluating it yields-
```
Program () (Version () 1 0 0) (Delay () (Constant () (Some (ValueOf bytestring "A"))))
```
The function application is "delayed". It will not be evaluated (and therefore computed) until it is *forced*.

Plutarch level function application is strict. All of your function arguments are evaluated **before** the function is called.

This is often undesirable, and you want to create a delayed term instead that you want to force *only* when you need to compute it.

You can force a previously delayed expression using pforce-
```hs
pforce $ let f = plam (\x -> x) in pdelay (f # phexByteStr "41")
```
It evaluates to-
```
Program () (Version () 1 0 0) (Constant () (Some (ValueOf bytestring "A")))
```

Delaying the argument to a Plutarch level function, within the function body, is not very useful - since the argument has been evaluated before the function body has been entered! Instead, you'll often notice usage of `pdelay` and `pforce` in Haskell level function arguments to simulate laziness-
```hs
pif' :: Term s (PBool :--> a :--> a :--> a)

-- | Lazy if-then-else.
pif :: Term s PBool -> Term s a -> Term s a -> Term s a
pif cond whenTrue whenFalse = pforce $ pif' # cond # pdelay whenTrue # pdelay whenFalse
```

`pif'` is a direct synonym to the `IfThenElse` Plutus Core builtin function. Of course, it evaluates its arguments strictly but you often want an if-then-else that doesn't evaluate both its branches - only the one for which the condition holds. So, `pif`, as a haskell level function can take in both branches (without any concept of evaluating them), delay them and *then* apply it to `pif'`. Finally, a `pforce` will force the yielded branch that was previously delayed.

Delay and Force will be one of your most useful tools while writing Plutarch. Make sure you get a grip on them!

# Data encoding and Scott encoding
In Plutus Core, there are really two (conflicting) ways to represent non-trivial ADTs- `Constr` data encoding, or Scott encoding. You can (you should!) only use one of these representations for your non-trivial types.

> Aside: What's a "trivial" type? The non-data builtin types! `PInteger`, `PByteString`, `PBuiltinList`, `PBuiltinPair`, and `PMap` (actually just a builtin list of builtin pairs).

`Constr` data is essentially a sum-of-products representation. However, it can only contain other `Data` values (not necessarily just `Constr` data, could be `I` data, `B` data etc.) as its fields. Plutus Core famously lacks the ability to represent functions using this encoding, and thus - `Constr` encoded values simply cannot contain functions.

> Note: You can find out more about the deep details of `Data`/`BuiltinData` at [plutonomicon](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md).

With that said, `Data` encoding is *ubiquitous* on the chain. It's the encoding used by the ledger api types, it's the type of the arguments that can be passed to a script on the chain etc. As a result, your datum and redeemers *must* use data encoding.

On the opposite (and conflicting) end, is scott encoding. [The internet](https://crypto.stanford.edu/~blynn/compiler/scott.html) can explain scott encoding way better than I can. But I'll be demonstrating scott encoding with an example anyway.

Firstly, what good is scott encoding? Well it doesn't share the limitation of not being able to contain functions! However, you cannot use scott encoded types within, for example, your datums and redeemers.

Briefly, scott encoding is a way to represent data with functions. The scott encoded representation of `Maybe a` would be-
```hs
(a -> b) -> b -> b
```
`Just 42`, for example, would be represented as this function-
```hs
\f _ -> f 42
```
Whereas `Nothing` would be represented as this function-
```hs
\_ n -> n
```

We covered construction. What about usage/deconstruction? That's also just as simple. Let's say you have a function, `foo :: Maybe Integer -> Integer`, it takes in a scott encoded `Maybe Integer`, adds `42` to its `Just` value. If it's `Nothing`, it just returns 0.
```hs
{-# LANGUAGE RankNTypes #-}

type Maybe a = forall b. (a -> b) -> b -> b

just :: a -> Maybe a
just x = \f _ -> f x

nothing :: Maybe a
nothing = \_ n -> n

foo :: Maybe Integer -> Integer
foo mb = mb (\x -> x + 42) 0
```
How does that work? Recall that `mb` is really just a function. Here's what the application of `f` would work like-
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
How cool is that?

This is the same recipe followed in the implementation of `PMaybe`. See its [PlutusType impl](#plutustype-pcon-and-pmatch) below!

# Unsafe functions
There are internal functions such as `punsafeCoerce`, `punsafeConstant` etc. that give you terms without their specific type. These **should not** be used by Plutarch users. It is the duty of the user of these unsafe functions to get the type right - and it is very easy to get the type wrong. You can easily make the type system believe you're creating a `Term s PInteger`, when in reality, you created a function.

Things will go very wrong during script evaluation if you do that kind of thing.

The good thing is that unsafe functions all have explicit indicators through the names, as long as you don't use any `punsafe*` functions - you should be fine!
