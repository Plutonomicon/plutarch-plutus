This document describes various core Plutarch usage concepts.

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

<details>
<summary> Table of Contents </summary>

- [Conditionals](#conditionals)
- [Recursion](#recursion)
- [Do syntax with `TermCont`](#do-syntax-with--termcont-)
- [Do syntax with `QualifiedDo` and `Plutarch.Monadic`](#do-syntax-with--qualifieddo--and--plutarchmonadic-)
- [Deriving typeclasses for `newtype`s](#deriving-typeclasses-for--newtype-s)
- [Deriving typeclasses with generics](#deriving-typeclasses-with-generics)
- [`plet` to avoid work duplication](#-plet--to-avoid-work-duplication)
- [Tracing](#tracing)
- [Raising errors](#raising-errors)
- [Unsafe functions](#unsafe-functions)

<small><i><a href='http://ecotrust-canada.github.io/markdown-toc/'>Table of contents generated with markdown-toc</a></i></small>


</details>


# Conditionals

You can simulate `if/then/else` at the Plutarch level using `pif`:

```haskell
pif :: Term s PBool -> Term s a -> Term s a -> Term s a
```

This has similar semantics to Haskell's `if/then/else`. That is, only the branch for which the predicate holds - is evaluated.

```haskell
pif (pconstant True) 1 2
```

The above evaluates to `1`, which has type `Term s PInteger`.

# Recursion

To emulate recursion in [UPLC (Untyped Plutus Core)](https://github.com/Plutonomicon/plutonomicon/blob/main/uplc.md), you need to use [the Y combinator](https://stackoverflow.com/questions/93526/what-is-a-y-combinator). Plutarch provides the Y combinator with the name `pfix`:

```haskell
pfix :: Term s (((a :--> b) :--> (a :--> b)) :--> (a :--> b))
```

It works as you would expect, though the type is scary. Think of it as the Haskell type:

```haskell
fix :: ((a -> b) -> (a -> b)) -> (a -> b)
```

The first argument is "self", or the function you want to recurse with.

The below example implements a Plutarch-level factorial function:

```haskell
import Plutarch.Prelude

pfac :: Term s (PInteger :--> PInteger)
pfac = pfix #$ plam f
  where
    f :: Term s (PInteger :--> PInteger) -> Term s PInteger -> Term s PInteger
    f self n = pif (n #== 1) n $ n * (self #$ n - 1)
-- (ignore the existence of non positives :D)
```

Note how `f` takes in a `self` and just recurses on it. All you have to do, is create a Plutarch level function by using `plam` on `f` and `pfix` the result - and that `self` argument will be taken care of for you.

# Do syntax with `TermCont`

Continuation functions like `pmatch`, `plet`, and `pletFields` aren't exactly the most convenient, are they? Fortunately, `TermCont` makes it much easier to use. `TermCont` is the familiar [`Cont`](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Cont.html) monad, specialized for Plutarch terms.

`TermCont @b s a` essentially represents `(a -> Term s b) -> Term s b`. `a` being the input to the continuation, and `Term s b` being the output. Notice the type application - `b` must have been brought into scope through another binding first.

Consider the snippet:

```hs
import Plutarch.Api.V1.Contexts
import Plutarch.Prelude

test :: Term s (PScriptPurpose :--> PUnit)
test = plam $ \x -> pmatch x $ \case
  PSpending _ -> ptrace "matched spending script purpose" $ pconstant ()
  _ -> ptraceError "pattern match failure"
```

That's rather ugly! [`pmatch`](./TYPECLASSES.md#plutustype-pcon-and-pmatch) takes in a continuation as its second argument. Can we make this a bit more ergonomic?

```hs
import Plutarch.Api.Contexts
import Plutarch.Prelude

pmatchC :: PlutusType a => Term s a -> TermCont s (a s)
pmatchC = tcont . pmatch

ptraceC :: Term s PString -> TermCont s ()
ptraceC s = tcont $ \f -> ptrace s (f ())

test :: Term s (PScriptPurpose :--> PUnit)
test = plam $ \x -> unTermCont $ do
  PSpending _ <- pmatchC x
  ptraceC "matched spending script purpose"
  pure $ pconstant ()
```

How cool is that? You can use regular `do` syntax on the `TermCont` monad. All the continuations are flattened! Just remember to `unTermCont` the result.

Furthermore, this is very similar to the `Cont` monad - it just operates on Plutarch level terms. This means you can draw parallels to utilities and patterns one would use when utilizing the `Cont` monad. Here's an example:

```hs
import Plutarch.Prelude

-- | Terminate with given value on empty list, otherwise continue with head and tail.
nonEmpty :: Term s r -> PList a s -> TermCont @r s (Term s a, Term s (PList a))
nonEmpty x0 list = tcont $ \k ->
  case list of
    PSCons x xs -> k (x, xs)
    PSNil -> x0

foo :: Term s (PList PInteger :--> PInteger)
foo = plam $ \l -> unTermCont $ do
  (x, xs) <- nonEmpty 0 =<< tcont (pmatch l)
  pure $ x + plength # xs
```

`foo` adds up the first element of the given list with the length of its tail. Unless the list was empty, in which case, it just returns 0. It uses continuations with the `do` syntax to elegantly utilize short circuiting!

# Do syntax with `QualifiedDo` and `Plutarch.Monadic`

There's another way of having `do` syntax. Though this one doesn't use a lawful monad. Instead, it uses `QualifiedDo` - and therefore requires GHC 9.

The `Plutarch.Monadic` module exports `>>=`, `>>`, and `fail` functions suitable to be used with `QualifiedDo`.

```hs
-- NOTE: REQUIRES GHC 9!
{-# LANGUAGE QualifiedDo #-}

import Plutarch.Api.Contexts
import qualified Plutarch.Monadic as P
import Plutarch.Prelude

f :: Term s (PScriptPurpose :--> PUnit)
f = plam $ \x -> P.do
  PSpending _ <- pmatch x
  ptrace "matched spending script purpose"
  pconstant ()
```

In essence, `P.do { x; y }` simply translates to `x y`; where `x :: a -> Term s b` and `y :: a`.

Similarly, `P.do { y <- x; z }` translates to `x $ \case { y -> z; _ -> ptraceError <msg> }`; where `x :: (a -> Term s b) -> Term s b`, `y :: a`, and `z :: Term s b`. Of course, if `y` is a fully exhaustive pattern match (e.g. singular constructor), the extra `_ -> ptraceError <msg>` case will not be generated at all and you'd simply get `x $ \y -> z`.

Finally, `P.do { x }` is just `x`.

# Deriving typeclasses for `newtype`s

If you're defining a `newtype` to an existing Plutarch type, like so:

```hs
newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
```

You ideally want to just have this `newtype` be represented as a `PByteString` under the hood. Therefore, all the typeclass instances of `PByteString` make sense for `PPubKeyHash` as well. In this case, you can simply derive all those typeclasses for your `PPubKeyHash` type as well! Via `DerivePNewtype`:

```hs
{-# LANGUAGE UndecidableInstances #-}

import Plutarch.Prelude

newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PPubKeyHash PByteString)
```

(yes, you need `UndecidableInstances` to derive `PlutusType`)

`DerivePNewtype` takes two type parameters. Both of them are Plutarch types (i.e. types with kind `PType`). The first one is the type you're deriving the instances for, while the second one is the _inner_ type (whatever `PPubKeyHash` is a newtype to).

> Note: It's important to note that the content of a `newtype` _that aims to be a Plutarch type_ (i.e. can be represented as a Plutarch term), must also be a Plutarch term. The type `PByteString s` simply doesn't exist in the Plutus Core world after compilation. It's all just `Term`s. So, when you say `Term s PPubKeyHash`, you're really just describing a `Term s PByteString` under the hood - since that's what it _is_ during runtime.

> Aside: You can access the inner type using `pto` (assuming it's a `PlutusType` instance). For example, `pto x`, where `x :: Term s PPubKeyHash`, would give you `Term s PByteString`. `pto` converts a [`PlutusType`](./TYPECLASSES.md#plutustype-pcon-and-pmatch) term to its inner type. This is very useful, for example, when you need to use a function that operates on bytestring terms, but all you have is a `Term s PPubKeyHash`. You _know_ it's literally a bytestring under the hood anyway - but how do you obtain that? Using `pto`!

Currently, `DerivePNewtype` lets you derive the following typeclasses for your Plutarch _types_:

- `PlutusType`
- `PIsData`
- `PEq`
- `POrd`
- `PIntegral`

You can also derive the following typeclasses for Plutarch _terms_:

- `Num`
- `Semigroup`
- `Monoid`

What does this mean? Well, `Num` would actually be implemented for `Term s a`, where `a` is a Plutarch type. For example, if you wanted to implement `Semigroup` for `Term s PPubKeyHash` (`Term s PByteString` already has a `Semigroup` instance), you can write:

```hs
{-# LANGUAGE StandaloneDeriving #-}

deriving via (Term s (DerivePNewtype PPubKeyHash PByteString)) instance Semigroup (Term s PPubKeyHash)
```

# Deriving typeclasses with generics

Plutarch also provides sophisticated generic deriving support for completely custom types. In particular, you can easily derive `PlutusType` for your own type:

```hs
import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch.Prelude

data MyType (a :: PType) (b :: PType) (s :: S)
  = One (Term s a)
  | Two (Term s b)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType)
```

> Note: This requires the `generics-sop` package.

This will use a [Scott encoding representation](./CONCEPTS.md#scott-encoding) for `MyType`, which is typically what you want. If you want to use [data encoding representation](./CONCEPTS.md#data-encoding) instead in your `PlutusType` instance - you should derive it using `PIsDataReprInstances`. Check out: [implementing `PIsDataRepr` and friends](./TYPECLASSES.md#implementing-pisdatarepr-and-friends)

Currently, generic deriving supports the following typeclasses:

- [`PlutusType`](./TYPECLASSES.md#implementing-plutustype-for-your-own-types-scott-encoding) (Scott encoding only)
- [`PIsDataRepr`](./TYPECLASSES.md#implementing-pisdatarepr-and-friends)

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

# Unsafe functions

There are internal functions such as `punsafeCoerce`, `punsafeConstant` etc. that give you terms without their specific type. These **should not** be used by Plutarch users. It is the duty of the user of these unsafe functions to get the type right - and it is very easy to get the type wrong. You can easily make the type system believe you're creating a `Term s PInteger`, when in reality, you created a function.

Things will go very wrong during script evaluation if you do that kind of thing.

The good thing is that unsafe functions all have explicit indicators through the names, as long as you don't use any `punsafe*` functions - you should be fine!

Of course, these have legitimate use cases. Most often, we use these functions to convert between types that _truly_ have the same internal representation in UPLC - but the type system simply isn't expressive enough to infer that.
