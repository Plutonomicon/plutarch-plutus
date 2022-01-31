> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

<details>
<summary> Table of Contents </summary>

- [Overview](#overview)
  - [Compiling and Running](#compiling-and-running)
    - [Common Extensions and GHC options](#common-extensions-and-ghc-options)
    - [Code](#code)
  - [Syntax](#syntax)
    - [Constants](#constants)
    - [Lambdas](#lambdas)
    - [Delayed terms and Forcing](#delayed-terms-and-forcing)
  - [Usage](#usage)
    - [Applying functions](#applying-functions)
    - [Conditionals](#conditionals)
    - [Recursion](#recursion)
    - [Do syntax with `QualifiedDo` and `Plutarch.Monadic`](#do-syntax-with-qualifieddo-and-plutarchmonadic)
      - [Translating `do` syntax to GHC 8](#translating-do-syntax-to-ghc-8)
    - [Deriving typeclasses for `newtype`s](#deriving-typeclasses-for-newtypes)
    - [Deriving typeclasses with generics](#deriving-typeclasses-with-generics)
  - [Concepts](#concepts)
    - [Hoisting, metaprogramming,  and fundamentals](#hoisting-metaprogramming--and-fundamentals)
      - [Hoisting Operators](#hoisting-operators)
    - [What is the `s`?](#what-is-the-s)
    - [eDSL Types in Plutarch](#edsl-types-in-plutarch)
    - [`plet` to avoid work duplication](#plet-to-avoid-work-duplication)
    - [Tracing](#tracing)
    - [Raising errors](#raising-errors)
    - [Delay and Force](#delay-and-force)
    - [Data encoding and Scott encoding](#data-encoding-and-scott-encoding)
    - [Unsafe functions](#unsafe-functions)
  - [Typeclasses](#typeclasses)
    - [Equality and Order](#equality-and-order)
    - [Monoids](#monoids)
    - [PIntegral](#pintegral)
    - [PIsData](#pisdata)
    - [PConstant & PLift](#pconstant--plift)
      - [Implementing `PConstant` & `PLift`](#implementing-pconstant--plift)
    - [PlutusType, PCon, and PMatch](#plutustype-pcon-and-pmatch)
      - [Implementing `PlutusType` for your own types](#implementing-plutustype-for-your-own-types)
    - [PListLike](#plistlike)
    - [PIsDataRepr](#pisdatarepr)
      - [Implementing PIsDataRepr](#implementing-pisdatarepr)
  - [Working with Types](#working-with-types)
    - [PInteger](#pinteger)
    - [PBool](#pbool)
    - [PString](#pstring)
    - [PByteString](#pbytestring)
    - [PUnit](#punit)
    - [PBuiltinList](#pbuiltinlist)
    - [PList](#plist)
    - [PBuiltinPair](#pbuiltinpair)
    - [PAsData](#pasdata)
    - [PDataSum & PDataList](#pdatasum--pdatalist)
    - [PData](#pdata)
    - [PRecord](#precord)
      - [letrec](#letrec)
      - [Record Data](#record-data)
- [Examples](#examples)
  - [Fibonacci number at given index](#fibonacci-number-at-given-index)
  - [Validator that always succeeds](#validator-that-always-succeeds)
  - [Validator that always fails](#validator-that-always-fails)
  - [Validator that checks whether a value is present within signatories](#validator-that-checks-whether-a-value-is-present-within-signatories)
  - [Using custom datum/redeemer in your Validator](#using-custom-datumredeemer-in-your-validator)
  - [Manually extracting fields from `ScriptContext` (UNTYPED)](#manually-extracting-fields-from-scriptcontext-untyped)
- [Thumb rules, Tips, and Tricks](#thumb-rules-tips-and-tricks)
  - [Plutarch functions are strict](#plutarch-functions-are-strict)
  - [Don't duplicate work](#dont-duplicate-work)
  - [Prefer Plutarch level functions](#prefer-plutarch-level-functions)
  - [When to use Haskell level functions?](#when-to-use-haskell-level-functions)
  - [Hoisting is great - but not a silver bullet](#hoisting-is-great---but-not-a-silver-bullet)
  - [The difference between `PlutusType`/`PCon` and `PLift`'s `pconstant`](#the-difference-between-plutustypepcon-and-plifts-pconstant)
- [Common Issues](#common-issues)
  - [No instance for (PUnsafeLiftDecl a)](#no-instance-for-punsafeliftdecl-a)
  - [Infinite loop / Infinite AST](#infinite-loop--infinite-ast)
  - [Couldn't match type `Plutarch.DataRepr.Internal.PUnLabel ...` arising from a use of `pfield` (or `hrecField`, or `pletFields`)](#couldnt-match-type-plutarchdatareprinternalpunlabel--arising-from-a-use-of-pfield-or-hrecfield-or-pletfields)
  - [Expected a type, but "fieldName" has kind `GHC.Types.Symbol`](#expected-a-type-but-fieldname-has-kind-ghctypessymbol)
  - [Lifting `PAsData`](#lifting-pasdata)
- [Useful Links](#useful-links)
</details>

# Overview

## Compiling and Running

### Common Extensions and GHC options

You generally want to adhere to the same extensions and GHC options the [Plutarch repo](https://github.com/Plutonomicon/plutarch/blob/master/plutarch.cabal) uses.

### Code

You can compile a Plutarch term using `compile`(from `Plutarch` module), making sure it has no free variables. `compile` returns a `Script`- you can use this as you would any other Plutus script. The API in `Plutus.V1.Ledger.Scripts` should prove helpful.

For further insight into what is compiled - you can use `printTerm` or `printScript` (from `Plutarch` module).

I often use these helper functions to test Plutarch quickly-

```haskell
import Data.Text (Text)
import Plutarch.Evaluate (evaluateScript)
import Plutarch (ClosedTerm, compile)
import Plutus.V1.Ledger.Api (ExBudget)
import Plutus.V1.Ledger.Scripts (Script (unScript), ScriptError, applyArguments)
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)
import PlutusTx (Data)

eval :: ClosedTerm a -> Either ScriptError (ExBudget, [Text], Program DeBruijn DefaultUni DefaultFun ())
eval x = fmap (\(a, b, s) -> (a, b, unScript s)) . evaluateScript $ compile x

evalWithArgs :: ClosedTerm a -> [Data] -> Either ScriptError (ExBudget, [Text], Program DeBruijn DefaultUni DefaultFun ())
evalWithArgs x args = fmap (\(a, b, s) -> (a, b, unScript s)) . evaluateScript . flip applyArguments args $ compile x
```

The fields in the result triple correspond to execution budget (how much memory and CPU units were used), trace log, and script result - respectively. Often you're only interested in the script result, in that case you can use-

```haskell
evalT :: ClosedTerm a -> Either ScriptError (Program DeBruijn DefaultUni DefaultFun ())
evalT x = fmap (\(_, _, s) -> unScript s) . evaluateScript $ compile x

evalWithArgsT :: ClosedTerm a -> [Data] -> Either ScriptError (Program DeBruijn DefaultUni DefaultFun ())
evalWithArgsT x args = fmap (\(_, _, s) -> unScript s) . evaluateScript . flip applyArguments args $ compile x
```

> Note: You can pretty much ignore the UPLC types involved here. All it really means is that the result is a "UPLC program". When it's printed, it's pretty legible - especially for debugging purposes. Although not necessary to use Plutarch, you may find the [Plutonomicon UPLC guide](https://github.com/Plutonomicon/plutonomicon/blob/main/uplc.md) useful.

## Syntax

A Plutarch script is a `Term`. This can consist of-

### Constants

These can be either built directly from Haskell synonyms using `pconstant` (requires [`PConstant`/`PLift`](#pconstant--plift) instance). `pconstant` always takes in a regular Haskell value to create its Plutarch synonym.
```hs
import Plutarch.Prelude

-- | A plutarch level boolean. Its value is "True", in this case.
x :: Term s PBool
x = pconstant True
```
> Aside: Sometimes, you might find `pconstant` raise "Ambiguous type variable" error. In this case, you should use `TypeApplications` to help GHC realize what **Plutarch type** you're trying to construct. e.g `pconstant @PInteger 42`

Or from Plutarch terms within other constructors using `pcon` (requires [`PlutusType`/`PCon`](#plutustype-pcon-and-pmatch) instance)-
```haskell
import Plutarch.Prelude

-- | Create a plutarch level optional value from given value.
f :: Term s (a :--> PMaybe a)
f = plam $ \x -> pcon $ PJust x
-- Note that 'PMaybe' has a 'PlutusType' instance.
```
> `PMaybe` declaration: `data PMaybe a s = PJust (Term s a) | PNothing`

> Aside: Notice that `pcon` actually takes in a Plutarch type to create a Plutarch term.
>
> In particular, `PJust x`, where `x :: Term s a`, has type `PMaybe a s`.
>
> ```hs
> -- Example
> > :t x
> Term s PInteger
> > :t PJust x
> PMaybe PInteger s
> > :t pcon (PJust x)
> Term s (PMaybe PInteger)
> ```
>
> Thus, within the `f` definition above, `pcon` has type `PMaybe a s -> Term s (PMaybe a)`. Similarly, `pcon PNothing` would yield `forall x. Term s (PMaybe x)`. Since `PNothing` has type `PMaybe x s`.
>
> ```hs
> -- Example
> > :t PNothing
> PMaybe a s
> > :t pcon PNothing
> Term s (PMaybe a)
> ```

Or by using literals-
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

Or by using other, miscellaneous functions provided by Plutarch-
```haskell
import qualified Data.ByteString as BS
import Plutarch.Prelude

-- | A plutarch level bytestring. Its value is [65], in this case.
x :: Term s PByteString
x = phexByteStr "41"
-- ^ 'phexByteStr' interprets a hex string as a bytestring. 0x41 is 65 - of course.
```

### Lambdas
You can create Plutarch level lambdas by apply `plam` over a Haskell level lambda/function.

```haskell
pid :: Term s (a :--> a)
pid = plam $ \x -> x
```

The identity function! Notice the type. A Plutarch level lambda uses the funny arrows `:-->` to encode a function type. In the above case, `pid` is a Plutarch level function that takes a type `a`, and returns the same type - `a`. As one would expect, `:-->` is right associative and things curry like a charm (at least, they should).

Guess what this Plutarch level function does-

```haskell
f :: Term s (PInteger :--> PString :--> a :--> a)
```

That's right! It takes in an integer, a string, and a type `a` and returns the same type `a`. Notice that all of those types are Plutarch level types.

This is the type of the Haskell level function, `plam`-

```haskell
plam :: (Term s a -> Term s b) -> Term s (a :--> b)
```

(That's actually a lie! But we are going to ignore the _real_  `plam` type for simplicity)

It just converts a Haskell level function, which operates on purely Plutarch terms, into a Plutarch level function.

This means that when faced with filling out the gap-

```haskell
f :: Term s (PInteger :--> PString :--> a :--> a)
f = plam $ \???
```

You know that the argument to `plam` here will just be a Haskell function that takes in - `Term s PInteger`, `Term s PString`, and `Term s a` (in  that order), and spits out a `Term s a` back.

### Delayed terms and Forcing
You can use `pdelay` to create a delayed term and `pforce` to force on it. More details at [Delay and Force](#delay-and-force).

## Usage

### Applying functions

You can apply Plutarch level functions using `#` and `#$` (or `papp`). Notice the associativity and precedence of those operators-

```haskell
infixl 8 #

infixr 0 #$
```

`#$` is pretty much just `$` for Plutarch functions. But `#` is left associative and has a high precedence. This essentially means that the following-

```haskell
f # 1 # 2

-- f :: Term s (PInteger :--> PInteger :--> PUnit)
```

applies `f` to 1 and 2. i.e, it is parsed as - `((f 1) 2)`

Whereas, the following-

```haskell
f #$ foo # 1

-- f :: Term s (PBool :--> PUnit)
-- foo :: Term s (PInteger :--> PBool)
```

parses as - `f (foo 1)`

(don't take the parens literally - after all `f` and `foo` are not Haskell level functions)

> Aside: Remember that function application here is **strict**. The arguments _will be evaluated_ and then passed in.
>
> Rule of thumb: If you see `#` - you can quickly infer that a Plutarch level function is being applied and the arguments will be evaluated. Haskell level functions still have their usual semantics, which is why `pif` doesn't evaluate both branches. (if you want, you can use `pif'` - which is a Plutarch level function and therefore strict)

### Conditionals

You can simulate `if/then/else` at the Plutarch level using `pif`-

```haskell
pif :: Term s PBool -> Term s a -> Term s a -> Term s a
```

This has similar semantics to Haskell's `if/then/else`. That is, only the branch for which the predicate holds - is evaluated.

```haskell
pif (pconstant True) 1 2
```

The above evaluates to `1`, which has type `Term s PInteger`

Of course, the predicate can be an arbitrary `Term s PBool` producing computation.

### Recursion

To emulate recursion in UPLC (Untyped Plutus Core), you need to use the Y combinator. Plutarch provides the Y combinator with the name `pfix`-

```haskell
pfix :: Term s (((a :--> b) :--> a :--> b) :--> a :--> b)
```

It works as you would expect, though the type is scary. Think of it as the Haskell type-

```haskell
fix :: ((a -> b) -> (a -> b)) -> a -> b
```

The first argument is &quot;self&quot;, or the function you want to recurse with.

```haskell
import Plutarch.Prelude

pfac :: Term s (PInteger :--> PInteger)
pfac = pfix #$ plam f
  where
    f :: Term s (PInteger :--> PInteger) -> Term s PInteger -> Term s PInteger
    f self n = pif (n #== 1) n $ n * (self #$ n - 1)
-- (ignore the existence of non positives :D)
```

There's a Plutarch level factorial function! Note how `f` takes in a `self` and just recurses on it. All you have to do, is create a Plutarch level function by using `plam` on `f` and `pfix` the result - and that `self` argument will be taken care of for you.

### Do syntax with `QualifiedDo` and `Plutarch.Monadic`
The `Plutarch.Monadic` module provides convenient do syntax on common usage scenarios. It requires the `QualifiedDo` extension, which is only available in GHC 9.

```hs
-- NOTE: REQUIRES GHC 9!
{-# LANGUAGE QualifiedDo #-}

import qualified Plutarch.Monadic as P

f :: Term s (PTxInfo :--> PBuiltinList (PAsData PTxInInfo))
f = plam $ \x -> P.do
  PTxInfo txInfoFields <- pmatch x
  ptrace "yielding first field from tx info"
  pfromData $ pdhead # txInfoFields
```
In essence, `P.do { x; y }` simply translates to `x y`; where `x :: (a -> Term s b) -> a -> Term s b` and `y :: a`.

Similarly, `P.do { y <- x; z }` translates to `x $ \case { y -> z; _ -> ptraceError <msg> }`. Of course, if `y` is a fully exhaustive pattern match (e.g, singular constructor), the extra `_ -> ptraceError <msg>` case will not be generated at all and you'd simply get `x $ \y -> z`.

Finally, `P.do { x }` is just `x`.

These semantics make it *extremely* convenient for usage of [`pmatch`](#plutustype-pcon-and-pmatch), [`ptrace`](#tracing) etc.
```hs
pmatch :: Term s a -> (a s -> Term s b) -> Term s b

ptrace :: Term s PString -> Term s a -> Term s a
```

#### Translating `do` syntax to GHC 8
For convenience, most examples in this guide will be utilizing this `do` syntax. However, since `QualifiedDo` is available pre GHC 9 - we'll discuss how to translate those examples to GHC 8.

There are three ways to do this-
* Use [`RebindableSyntax`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rebindable_syntax.html). You can replace the `>>=`, `>>`, and `fail` functions in your scope with the ones from `Plutarch.Monadic` using `RebindableSyntax`. This is arguably a bad practice but the choice is there. This will let you use the `do` syntax word for word. Although you wouldn't be qualifying your `do` keyword (like `P.do`), you'd just be using `do`.
* Use the `Cont` monad. You can utilize this to also use regular `do` syntax by simply applying `cont` over functions such as `pmatch`, `pletFields` and similar utilities that take in a continuation function. There is an example of this [here](./../examples/Examples/Api.hs). Notice how `checkSignatory` has been translated to `Cont` monad usage in `checkSignatoryCont`.
* Don't use do syntax at all. You can easily translate the `do` syntax to regular continuation chains. Here's how you'd translate the above `f` function-

  ```hs
  f :: Term s (PTxInfo :--> PBuiltinList (PAsData PTxInInfo))
  f = plam $ \x -> pmatch x $ \(PTxInfo txInfoFields) ->
    ptrace "yielding first field from tx info" $ pfromData $ pdhead # txInfoFields
  ```
  Simply put, functions like `pmatch`, `pletFields` take in a continuation. The `do` syntax enables you to bind the argument of the continuation using `<-`, and simply use flat code, rather than nested function calls.

### Deriving typeclasses for `newtype`s
If you're defining a `newtype` to an existing Plutarch type, like so-
```hs
newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
```
You ideally want to just have this `newtype` be represetned as a `PByteString` under the hood. Therefore, all the typeclass instances of `PByteString` make sense for `PPubKeyHash` as well. In this case, you can simply derive all those typeclasses for your `PPubKeyHash` type as well! Via `DerivePNewtype`-
```hs
newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PPubKeyHash PByteString)
```
`DerivePNewtype` takes two type parameters. Both of them are Plutarch types (i.e types with kind `PType`). The first one is the type you're deriving the instances for, while the second one is the *inner* type (whatever `PPubKeyHash` is a newtype to).

> Note: It's important to note that the contents of a `newtype` *that aims to be a Plutarch type* (i.e can be represented as a Plutarch term), must also be Plutarch terms. The type `PByteString s` simply doesn't exist in the Plutus Core world after compilation. It's all just `Term`s. So, when you say `Term s PPubKeyHash`, you're really just describing a `Term s PByteString` under the hood - since that's what it *is* during runtime.

> Aside: You can access the inner type using `pto` (assuming it's a `PlutusType` instance). For example, `pto x`, where `x :: Term s PPubKeyHash`, would give you `Term s PByteString`. `pto` converts a [`PlutusType`](#plutustype-pcon-and-pmatch) term to its inner type. This is very useful, for example, when you need to use a function that operates on bytestring terms, but all you have is a `Term s PPubKeyHash`. You *know* it's literally a bytestring under the hood anyway - but how do you obtain that? Using `pto`!

Currently, `DerivePNewType` can let you derive the following typeclasses for your Plutarch *types*:-
* `PlutusType`
* `PIsData`
* `PEq`
* `POrd`
* `PIntegral`

You can also derive the following typeclasses for Plutarch *terms*:-
* `Num`
* `Semigroup`
* `Monoid`

What does this mean? Well, `Num` would actually be implemented for `Term s a`, where `a` is a Plutarch type. For example, if you wanted to implement `Semigroup` for `Term s PPubKeyHash` (`Term s PByteString` already has a `Semigroup` instance), you can write-
```hs
{-# LANGUAGE StandaloneDeriving #-}

deriving via (Term s (DerivePNewtype PPubKeyHash PByteString)) instance Semigroup (Term s PPubKeyHash)
```

### Deriving typeclasses with generics
Plutarch also provides sophisticated generic deriving support for completely custom types. In particular, you can easily derive `PlutusType` for your own type-
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
This will use a [scott encoding representation](TODO - Scott encoding) for `MyType`, which is typically what you want. However, this will forbid you from representing your type as a `Data` value and as a result - you cannot implement `PIsData` for it. (Well, you can if you try hard enough - but you *really really really* shouldn't)

Currently, generic deriving supports the following typeclasses:-
* [`PlutusType`](#implementing-plutustype-for-your-own-types) (scott encoding only)
* [`PIsDataRepr`](#implementing-pisdatarepr)

## Concepts

### Hoisting, metaprogramming,  and fundamentals

What is essentially happening here, is that we have a 2-stage compilation process.

First GHC compiles our code, then our code generates an _AST_ of our Plutus script,

which is then serialized using `compile`.

The important thing to note, is that when you have a definition like:

```haskell
x :: Term s PInteger
x = something complex
```

Any use of `x` will inline the **full definition** of `x`. `x + x` will duplicate `something complex` in the AST. To avoid this, you should [use `plet` in order to avoid duplicate work](#plet-to-avoid-work-duplication). Do note that this is **strictly evaluated, and hence isn't always the best solution.**

There is however still a problem: What about top-level functions, like `fib`, `sum`, `filter`, and such? We can use `plet` to avoid duplicating the definition, but this error-prone, since to do this perfectly each function that generates part of the AST would need to have access to the `plet`'ed definitions, meaning that we'd likely have to put it into a record or typeclass.

To solve this problem, Plutarch supports _hoisting_. Hoisting only works for _closed terms_, that is, terms that don't reference any free variables (introduced by `plam`).

Hoisted terms are essentially moved to a top-level `plet`, i.e. it's essentially common subexpression elimination. Do note that because of this, your hoisted term is **also strictly evaluated, meaning that you shouldn't hoist non-lazy complex computations (use e.g.** `pdelay` **to avoid this).**

#### Hoisting Operators
For the sake of convenience, you often would want to use operators - which must be Haskell level functions. This is the case for `+`, `-`, `#==` and many more.

Choosing convenience over efficiency is difficult, but if you notice that your operator uses complex logic and may end up creating big terms - you can trivially factor out the logic into a Plutarch level function, hoist it, and simply apply that function within the operator.

Consider boolean or-
```hs
(#||) :: Term s PBool -> Term s PBool -> Term s PBool
x #|| y = pif x (pconstant PTrue) $ pif y (pconstant PTrue) $ pconstant PFalse
```
You can factor out most of the logic to a Plutarch level function, and apply that in the operator definition-

```hs
(#||) :: Term s PBool -> Term s PBool -> Term s PBool
x #|| y = por # x # pdelay y

por :: Term s (PBool :--> PDelayed PBool :--> PBool)
por = phoistAcyclic $ plam $ \x y -> pif' # x # pconstant PTrue # pforce y
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

### What is the `s`?

The `s` essentially represents the context, and is like the `s` of `ST`.

It's used to distinguish between closed and open terms:

- Closed term: `type ClosedTerm = forall s. Term s a`
- Arbitrary term: `exists s. Term s a`
- NB: `(exists s. Term s a) -> b` is isomorphic to
- `forall s. Term s a → b`

### eDSL Types in Plutarch

Most types prefixed with `P` are eDSL-level types, meaning that they're meant to be used with `Term`. They are merely used as a tag, and what Haskell value they can hold is not important. Their kind must be `(k → Type) → Type` .

### `plet` to avoid work duplication
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

### Tracing
You can use the functions `ptrace`, `ptraceError`, `ptraceIfFalse`, `ptraceIfTrue` (from `Plutarch.Trace`) for tracing. These behave similarly to the ones you're used to from [PlutusTx](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Trace.html).

If you have the `development` flag for `plutarch` turned on - you'll see the trace messages appear in the trace log during script evaluation. When not in development mode - these functions basically do nothing.

### Raising errors
In Plutus Tx, you'd signal validation failure with the [`error`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Prelude.html#v:error) function. You can do the same in Plutarch using `perror`.
```hs
fails :: Term s (PData :--> PData :--> PData :--> PUnit)
fails = plam $ \_ _ _ -> perror
```

### Delay and Force
Use `pdelay` on a term to create a "delayed term".
```hs
let f = plam (\x -> x) in pdelay (f # phexByteStr 0x41)
```
Compiling and evaluating it yields-
```
Program () (Version () 1 0 0) (Delay () (Constant () (Some (ValueOf bytestring "A"))))
```
The function application is "delayed". It will not be evaluated (and therefore computed) until it is *forced*.

Plutarch level function application is strictly evaluated in Plutarch.
All of your function arguments are evaluated **before** the function is called.

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

### Data encoding and Scott encoding
In Plutus Core, there are really two ways to represent non-trivial ADTs.

### Unsafe functions
There are internal functions such as `punsafeCoerce`, `punsafeConstant` etc. that give you terms without their specific type. These **should not** be used by Plutarch users. It is the duty of the user of these unsafe functions to get the type right - and it is very easy to get the type wrong. You can easily make the type system believe you're creating a `Term s PInteger`, when in reality, you created a function.

Things will go very wrong during script evaluation if you do that kind of thing.

The good thing is that unsafe functions all have explicit indicators through the names, as long as you don't use any `punsafe*` functions - you should be fine!

## Typeclasses

### Equality and Order

Plutarch level equality is provided by the `PEq` typeclass-

```haskell
class PEq t where
  (#==) :: Term s t -> Term s t -> Term s PBool
```

`PInteger` implements `PEq` as you would expect. So you could do-

```haskell
1 #== 2
```

That would yield a `Term s PBool`, which you would probably use with `pif` (or similar)

There is also a synonym to  `Ord`, `POrd`-

```haskell
class POrd t where
  (#<=) :: Term s t -> Term s t -> Term s PBool
  (#<) :: Term s t -> Term s t -> Term s PBool
```

It works as you would expect-

```haskell
{-# LANGUAGE OverloadedStrings #-}

pif (1 #< 7) "indeed" "what"
```

evaluates to `"indeed"` - of type `Term s PString`.

### Monoids

You can use `<>` on two `Term s a`s to produce one `Term s a`, where `Term s a` is a `Semigroup`. You can use `mempty` to create a `Term s a`, where `Term s a` is a monoid.

It works the way you would expect, `PByteString` and `PString` terms have `Monoid` instances-

```haskell
{-# LANGUAGE OverloadedStrings #-}

"ab" <> "cd"
-- evaluates to "abcd"
```

Where all those strings are actually `Term s PString`s.

### PIntegral
This is similar to the `Integral` typeclass. However, it only has the following class methods-
* `pdiv` - similar to `div`
* `pmod` - similar to `mod`
* `pquot` - similar to `quot`
* `prem` - similar to `prem`

Using these functions, you can do division/modulus etc on Plutarch level values-
```hs
pdiv # 6 # 3
```
where `6` and `3` are `Term s PInteger`s yields `2` - also a `Term s PInteger`.

### PIsData
The `PIsData` typeclass facilitates easy and type safe conversion between Plutarch types and their corresponding `PData` representation - i.e [`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md). It keeps track of the type information through [`PAsData`](#pasdata).
```hs
class PIsData a where
  pfromData :: Term s (PAsData a) -> Term s a
  pdata :: Term s a -> Term s (PAsData a)
```

[`PInteger`](#pinteger) has a `PIsData` instance. The `PData` representation of `PInteger` is, of course, an `I` data. And you can get the `PInteger` back from an `I` data using `UnIData` (i.e `pasInt`).
```hs
instance PIsData PInteger where
  pfromData x = pasInt # pforgetData x
  pdata x = punsafeBuiltin PLC.IData # x
```
In essence, `pdata` wraps a `PInteger` into an `I` data value. Wheras `pfromData` simply unwraps the `I` data value to get a `PInteger`.

> Aside: You might be asking, what's an "`I` data value"? This is referring to the different constructors of `Data`/`BuiltinData`. You can find a full explanation of this at [plutonomicon](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md).

For the simple constructors that merely wrap a builtin type into `Data`, e.g integers, bytestrings, lists, and map, `PIsData` works in much the same way as above. However, what about `Constr` data values? When you have an ADT that doesn't correspond to those simple builtin types directly - but you still need to encode it as `Data` (e.g `PScriptContext`). In this case, you should [implement `PIsDataRepr`](#implementing-pisdatarepr) and you'll get the `PIsData` instance for free!

### PConstant & PLift
These 2 closely tied together typeclasses establish a bridge between a Plutarch level type (that is represented as a builtin type, i.e [`DefaultUni`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-core/html/PlutusCore.html#t:DefaultUni)) and its corresponding Haskell synonym. The gory details of these two are not too useful to users, but you can read all about it if you want at [Developers' corner](TODO: LINK - to PLift developer's guide).

What's more important, are the abilities that `PConstant`/`PLift` instances have-
```hs
pconstant :: PLift p => PLifted p -> Term s p

plift :: (PLift p, HasCallStack) => ClosedTerm p -> PLifted p
```
> Aside: `PLifted p` represents the Haskell synonym to the Plutarch type, `p`. Similarly, there is also `PConstanted h` - which represents the Plutarch synonym corresponding to the Haskell type, `h`. These type families may only be used for Plutarch types implementing `PConstant`/`PLift`.

`pconstant` lets you build a Plutarch value from its corresponding Haskell synonym. For example, the haskell synonym of [`PBool`](#pbool) is [`Bool`](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Bool.html#t:Bool).
```hs
b :: Term s PBool
b = pconstant False
```
Other than simple builtin types - you can also use `pconstant` to create [`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md) values! Usually, you'll want to keep the type information though - so here's an example of creating a `PScriptPurpose` from a familiar `ScriptPurpose` constant-
```hs
import Plutus.V1.Ledger.Contexts

purp :: Term s PScriptPurpose
purp = pconstant $ Minting ""
```

On the other end, `plift` lets you obtain the Haskell synonym of a Plutarch value (that is represented as a builtin value, i.e [`DefaultUni`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-core/html/PlutusCore.html#t:DefaultUni))-
```hs
import Plutus.V1.Ledger.Contexts

purp :: Term s PScriptPurpose
purp = pconstant $ Minting "be"

> plift purp
Minting "be"
```

#### Implementing `PConstant` & `PLift`
If your custom Plutarch type is represented by a builtin type under the hood (i.e not scott encoded - rather [`DefaultUni`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-core/html/PlutusCore.html#t:DefaultUni)) - you can easily implement `PLift` for it by using the provided machinery.

This comes in 3 flavors.
* Plutarch type represented **directly** by a builtin type that **is not** `Data` (`DefaultUniData`) ==> `DerivePConstantDirect`

  Ex: `PInteger` is directly represented as a builtin integer.
* Plutarch type represented **indirectly** by a builtin type that **is not** `Data` (`DefaultUniData`) ==> `DerivePConstantViaNewtype`

  Ex: `PPubKeyHash` is a newtype to a `PByteString`, `PByteString` is *directly* represented as a builtin bytestring.
* Plutarch type represented by `Data` (`DefaultUniData`) ==> `DerivePConstantViaData`

  Ex: `PScriptPurpose` is represented as a `Data` value. It is synonymous to `ScriptPurpose` from the Plutus ledger api.

Whichever path you need to go down, there is one common part- implementing `PLift`, or rather `PUnsafeLiftDecl`. See, `PLift` is actually just a type synonym to `PUnsafeLiftDecl`. Essentially an empty typeclass with an associated type family that provides insight on the relationship between a Plutarch type and its Haskell synonym.
```hs
instance PUnsafeLiftDecl YourPlutarchType where type PLifted YourPlutarchType = YourHaskellType
```
You're tasked with assigning the correct Haskell synonym to your Plutarch type, and what an important task it is! Recall that `pconstant`'s argument type will depend on your assignment here. In particular: `pconstant :: YourHaskellType -> YourPlutarchType`.

Some examples:-
* for `YourPlutarchType` = `PInteger`, `YourHaskellType` = `Integer`

  ```hs
  instance PUnsafeLiftDecl PInteger where type PLifted PInteger = Integer
  ```
* for `YourPlutarchType` = `PValidatorHash`, `YourHaskellType` = `ValidatorHash`

  ```hs
  instance PUnsafeLiftDecl PValidatorHash where type PLifted PValidatorHash = Plutus.ValidatorHash
  ```
* for `YourPlutarchType` = `PScriptPurpose`, `YourHaskellType` = `ScriptPurpose`

  ```hs
  instance PUnsafeLiftDecl PScriptPurpose where type PLifted PScriptPurpose = Plutus.ScriptPurpose
  ```

Now, let's get to implementing `PConstant` for the Haskell synonym, via the 3 methods. The first of which is `DerivePConstantDirect`-
```hs
deriving via (DerivePConstantDirect Integer PInteger) instance (PConstant Integer)
```
`DerivePConstantDirect` takes in 2 type parameters-
* The Haskell type itself, for which `PConstant` is being implemented for.
* The **direct** Plutarch synonym to the Haskell type.

Pretty simple! Let's check out `DerivePConstantViaNewtype` now-
```hs
import qualified Plutus.V1.Ledger.Api as Plutus

newtype PValidatorHash (s :: S) = PValidatorHash (Term s PByteString)

...

deriving via (DerivePConstantViaNewtype Plutus.ValidatorHash PValidatorHash PByteString) instance (PConstant Plutus.ValidatorHash)
```
`DerivePConstantViaNewtype` also takes in 3 type parameters-
* The Haskell newtype itself, for which `PConstant` is being implemented for.
* The Plutarch synonym to the Haskell type.
* The actual Plutarch type corresponding to the Haskell type contained within the newtype.

  E.g `ValidatorHash` is a newtype to a `ByteString`, which is synonymous to `PByteString`. In the same way, `PValidatorHash` is actually just a newtype to a `PByteString` term.
During runtime, `ValidatorHash` is actually just a `ByteString`, the same applies for `PValidatorHash`. So we give it the `newtype` treatment with `DerivePConstantViaNewtype`!

Finally, we have `DerivePConstantViaData` for `Data` values-
```hs
import qualified Plutus.V1.Ledger.Api as Plutus

data PScriptPurpose (s :: S)
  = PMinting (Term s (PDataList '[PCurrencySymbol]))
  | PSpending (Term s (PDataList '[PTxOutRef]))
  | PRewarding (Term s (PDataList '[PStakingCredential]))
  | PCertifying (Term s (PDataList '[PDCert]))

...

deriving via (DerivePConstantViaData Plutus.ScriptPurpose PScriptPurpose) instance (PConstant Plutus.ScriptPurpose)
```
`DerivePConstantViaData` takes in 2 type parameters-
* The Haskell type itself, for which `PConstant` is being implemented for.
* The Plutarch synonym to the Haskell type.
And that's all you need to know to implement `PConstant` and `PLift`!

### PlutusType, PCon, and PMatch
`PlutusType` lets you construct and deconstruct Plutus Core constants from from a Plutarch type's constructors (possibly containing other Plutarch terms). It's essentially a combination of `PCon` (for constant construction) and `PMatch` (for constant deconstruction).

```hs
class (PCon a, PMatch a) => PlutusType (a :: k -> Type) where
  type PInner a (b' :: k -> Type) :: k -> Type
  pcon' :: forall s. a s -> forall b. Term s (PInner a b)
  pmatch' :: forall s c. (forall b. Term s (PInner a b)) -> (a s -> Term s c) -> Term s c
```
`PInner` is meant to represent the "inner" type of `a` - the Plutarch type representing the Plutus Core constant used to represent `a`.

Here's the `PlutusType` instance for `PMaybe`-
```hs
data PMaybe a s = PJust (Term s a) | PNothing

instance PlutusType (PMaybe a) where
  type PInner (PMaybe a) b = (a :--> b) :--> PDelayed b :--> b
  pcon' :: forall s. PMaybe a s -> forall b. Term s (PInner (PMaybe a) b)
  pcon' (PJust x) = plam $ \f (_ :: Term _ _) -> f # x
  pcon' PNothing = plam $ \_ g -> pforce g
  pmatch' x f = x # (plam $ \inner -> f (PJust inner)) # (pdelay $ f PNothing)
```
This is a scott encoded representation of the familiar `Maybe` data type. As you can see, `PInner` of `PMaybe` is actually a Plutarch level function. And that's exactly why `pcon'` creates a *function*. `pmatch'`, then, simply "matches" on the function - scott encoding fashion.

> Aside: Notice how `PJust` contains Plutarch term. This is where `PlutusType` is especially useful - for building up Plutarch terms *dynamically* - i.e, from arbitrary Plutarch terms.
>
> You should prefer `pconstant` (from [`PConstant`/`PLift`](#pconstant--plift)) when you can build something up entirely from Haskell level constants.

You should always use `pcon` and `pmatch` instead of `pcon'` and `pmatch'` - these are provided by the `PCon` and `PMatch` typeclasses-
```hs
class PCon a where
  pcon :: a s -> Term s a

class PMatch a where
  pmatch :: Term s a -> (a s -> Term s b) -> Term s b
```

All `PlutusType` instances get `PCon` and `PMatch` instances for free!

For types that cannot easily be both `PCon` and `PMatch` - feel free to implement just one of them! However, in general, **prefer implementing PlutusType**!

#### Implementing `PlutusType` for your own types
TODO

### PListLike
The `PListLike` typeclass bestows beautiful, and familiar, list utilities to its instances. Plutarch has two list types- [`PBuiltinList`](#pbuiltinlist) and [`PList`](#plist). Both have `PListLike` instances! However, `PBuiltinList` can only contain builtin types. It cannot contain Plutarch functions. The element type of `PBuiltinList` can be constrained using `PLift a => PBuiltinList a`.

> Note: `PLift` is exported from `Plutarch.Lift`.

As long as it's a `PLift a => PBuiltinList a` or `PList a` - it has access to all the `PListLike` goodies, out of the box. It helps to look into some of these functions at [`Plutarch.List`](./../Plutarch/List.hs).

Along the way, you might be confronted by 2 big mean baddies ...err, constraints-
```hs
PIsListLike list a
```
This just means that the type `list a`, is *indeed* a valid `PListLike` containing valid elements! Of course, all `PList a`s are valid `PListLike`, but we have to think about `PBuiltinList` since it can only contain `PLift a => a` elements! So, in essence a function declared as-
```hs
pfoo :: PIsListLike list a => Term s (list a :--> list a)
```
when specialized to `PBuiltinList`, can be simplified as-
```hs
pfoo :: PLift a => Term s (PBuiltinList a :--> PBuiltinList a)
```
That's all it is. Don't be scared of it!

What about this one-
```hs
PElemConstraint list a
```
This one ensures that the element type `a` can indeed be contained within the list type - `list`. For `PList`, this constraint means nothing - it's always true. For `PBuiltinList`, it can be simplified as `PLift a`. Easy!

Here's two of my favorite `PListLike` utilities (not biased)-
```hs
-- | Cons an element onto an existing list.
pcons :: PElemConstraint list a => Term s (a :--> list a :--> list a)

-- | The empty list
pnil :: PElemConstraint list a => Term s (list a)
```
What would life be without cons and nil?

Let's build a `PBuiltinList` of `PInteger`s with that-
```hs
x :: Term s (PBuiltinList PInteger)
x = pcons # 1 #$ pcons # 2 #$ pcons # 3 # pnil
```
Wooo! Let's not leave `PList` alone in the corner though-
```hs
x :: Term s (PList PInteger)
x = pcons # 1 #$ pcons # 2 #$ pcons # 3 # pnil
```
The code is the same, we just changed the type annotation. Cool!

### PIsDataRepr
`PIsDataRepr` and `PDataList` are the user-facing parts of an absolute workhorse of a machinery for easily deconstructing `Constr` [`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md) values. It allows fully type safe matching on `Data` values, without embedding type information within the generated script - unlike PlutusTx.

For example, `PScriptContext` - which is the Plutarch synonym to [`ScriptContext`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Contexts.html#t:ScriptContext) - has a `PIsDataRepr` instance, this lets you easily keep track of its type and match on it-
```hs
import Plutarch.Prelude
import Plutarch.Api.Contexts

foo :: Term s (PScriptContext :--> PString)
foo = plam $ \x -> pmatch x $ \(PScriptContext te) -> let purpose = pfromData $ pdhead #$ pdtail # te
  in pmatch purpose $ \case
    PMinting _ -> "It's minting!"
    PSpending _ -> "It's spending!"
    PRewarding _ -> "It's rewarding!"
    PCertifying _ -> "It's certifying!"
```
Of course, just like `ScriptContext` - `PScriptContext` is represented as a `Data` value in Plutus Core. Plutarch just lets you keep track of the *exact representation* of it within the type system.

First, we `pmatch` on `PScriptContext`-
```hs
pmatch :: Term s PScriptContext -> (PScriptContext s -> a) -> a
```
This allows us to pass in a Haskell function that works directly on the `PScriptContext` type, which is a familiar Haskell ADT-
```hs
data PScriptContext s = PScriptContext (Term s (PDataList '[PTxInfo, PScriptPurpose]))
```
We can match on that constructor and bind its field to `te`. Now, `te` is a Plutarch term - of course. But notice the `PDataList '[PTxInfo, PScriptPurpose]`. This is a heterogenous list! It represents all the fields in `PScriptContext` in order. Compare it to the real `ScriptContext`-
```hs
data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }
```
So, `te` is a *essentially* a Plutarch level heterogenous lists of fields. All of these fields are actually just `Data` (`PData`) under the hood - of course. You take apart a `PDataList` using `pdhead` and `pdtail`.

We are interested in the second field, `PScriptPurpose`, synonymous to [`ScriptPurpose`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Contexts.html#t:ScriptPurpose). So we do `pdhead #$ pdtail # te`. This gives us a `PAsData PScriptPurpose`. Finally a `pfromData` will get you the `PScriptPurpose` directly. This is because `PScriptPurpose` has a `PIsData` instance! It is a `Data` value under the hood, after all.

With that, you have `purpose :: Term s PScriptPurpose`. You can now `pmatch` on it and much like before, get at its Haskell level constructors!
`PScriptPurpose` looks like-
```hs
data PScriptPurpose s
  = PMinting (Term s (PDataList '[POpaque]))
  | PSpending (Term s (PDataList '[POpaque]))
  | PRewarding (Term s (PDataList '[POpaque]))
  | PCertifying (Term s (PDataList '[POpaque]))
```
> Aside: Ignore the `POpaque` - it's subject to change as the types are filled in.

Compare that to the real `ScriptPurpose`-
```hs
data ScriptPurpose
  = Minting CurrencySymbol
  | Spending TxOutRef
  | Rewarding StakingCredential
  | Certifying DCert
```
Cool! You now know how to use `pmatch` to get that convenient matching on your fully typed `Data` handling!

Let's pass in a `ScriptContext` as a `Data` value from Haskell to this Plutarch script and see if it works!
```hs
import Plutus.V1.Ledger.Api

mockCtx :: ScriptContext
mockCtx =
  ScriptContext
    (TxInfo
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      (interval (POSIXTime 1) (POSIXTime 2))
      mempty
      mempty
      ""
    )
    (Minting (CurrencySymbol ""))

> foo `evalWithArgsT` [PlutusTx.toData mockCtx]
Right (ExBudget {exBudgetCPU = ExCPU 4293277, exBudgetMemory = ExMemory 9362},[],Program () (Version () 1 0 0) (Constant () (Some (ValueOf string "It's minting!"))))
```

#### Implementing PIsDataRepr
If you have a custom ADT that will actually be represented as a `Data` value (`PData`) under the hood, implementing `PIsDataRepr` for your ADT (and **all its fields**!) is all you need to get convenient type tracking throughout its usage. This is going to be your biggest weapon when making custom datums and redeemers!

TODO

## Working with Types

### PInteger
`Term s PInteger` has a convenient `Num` instance that allows you to construct Plutarch level integer terms from regular literals. It also means you have all the typical arithmetic operations available to you-

```haskell
1 + 2
```

where `1` and `2` are `Term s PInteger`s.

Alongside `Num`, it also has a `PIntegral` instance, allowing you to division, modulus etc.

It also has a `PEq` and `POrd` instance, allowing you to do Plutarch level equality and comparison.

It **does not** have a `PlutusType` instance.

This is synonymous to Plutus Core [builtin integer](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html#t:Integer).

### PBool

Plutarch level boolean terms can be constructed using `pconstant True` and `pconstant False`.

```haskell
pif (pconstant PFalse) 7 42
-- evaluates to 42
```
You can combine Plutarch booleans terms using `#&&` and `#||`, which are synonyms to `&&` and `||`. These are haskell level operators and therefore have short circuiting. If you don't need short circuiting, you can use the Plutarch level alternatives- `pand'` and `por'` respectively.

This is synonymous to Plutus Core [builtin boolean](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins-Internal.html#t:BuiltinBool).

### PString

`Term s PString` has a `IsString` instance. This allows you to make Plutarch level string terms from regular string literals, provided you have `OverloadedStrings` turned on.

```haskell
{-# LANGUAGE OverloadedStrings #-}

"foo"
```

where &quot;foo&quot; is actually `Term s PString`.

It also has a `PEq` instance. And its terms have  `Semigroup` and `Monoid` instances - which work the way you would expect.

It **does not** have a `PlutusType` instance.

This is synonymous to Plutus Core [builtin string](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html#t:BuiltinString) (actually Text).

### PByteString

Plutarch level bytestring terms can be created using `phexByteStr` and `pbyteStr`. `phexByteStr` interprets a hex string literal as a `Term s PByteString` and `pbyteStr` merely converts a `ByteString` into a `Term s PByteString`.

```haskell
import qualified Data.ByteString as BS

phexByteStr "41"
-- yields a `Term s PByteString`, which represents [65]

pbyteStr (BS.pack [91])
-- yields a `Term s PByteString`, which represents [91]
```

Similar to `PString`, it has a `PEq` instance. As well as `Semigroup` and `Monoid` instances for its terms.

It **does not** have a `PlutusType` instance.

This is synonymous to Plutus Core [builtin bytestring](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html#t:BuiltinByteString).

### PUnit

The Plutarch level unit term can be constructed using `pconstant ()`.

This is synonymous to Plutus Core [builtin unit](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins-Internal.html#t:BuiltinUnit).

### PBuiltinList
You'll be using builtin lists quite a lot in Plutarch. `PBuiltinList` has a [`PListLike`](#plistlike) instance, giving you access to all the goodies from there! However, `PBuiltinList` can only contain builtin types. In particular, it cannot contain Plutarch functions.

You can express the constraint of "only builtin types" using `PLift`, exported from `Plutarch.Builtin`-`
```hs
validBuiltinList :: PLift a => PBuiltinList a
```
As mentioned before, `PBuiltinList` gets access to all the `PListLike` utilities. Other than that, `PLift a => PBuiltinList a` also has a [`PlutusType`](#plutustype-pcon-and-pmatch) instance. You can construct a `PBuiltinList` using `pcon` (but you should prefer using `pcons` from `PListLike`)-
```hs
> pcon $ PCons (phexByteStr "fe") $ pcon PNil
```
would yield a `PBuiltinList PByteString` with one element - `0xfe`. Of course, you could have done that with `pcons # phexByteStr "fe" # pnil` instead!

You can also use `pmatch` to match on a list-
```hs
pmatch (pcon $ PCons (phexByteStr "fe") $ pcon PNil) $ \case
  PNil -> "hey hey there's nothing here!"
  PCons _ _ -> "oooo fancy!"
```
But you should prefer `pelimList` from `PListLike` instead-
```hs
pelimList (\_ _ -> "oooo fancy") "hey hey there's nothing here!" $ pcon $ PCons (phexByteStr "fe") $ pcon PNil
```
The first argument is a function that is invoked for the `PCons` case, with the head and tail of the list as arguments.

The second argument is the value to return when the list is empty. It's *only evaluated* **if the list is empty**.

The final argument is, of course, the list itself.

> Aside: Interested in the lower level details of `PBuiltinList` (i.e Plutus Core builtin lists)? You can find all you need to know about it at [plutonomicon](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-lists.md).

### PList
Here's the scott encoded cousin of `PBuiltinList`. What does that mean? Well, in practice, it just means that `PList` can contain *any arbitrary* term - not just builtin types. `PList` also has a [`PListLike`](#plistlike) instance - so you won't be missing any of those utilities here!

`PList` also has a [`PlutusType`](#plutustype-pcon-and-pmatch) instance. You can construct a `PList` using `pcon` (but you should prefer using `pcons` from `PListLike`)-
```hs
> pcon $ PSCons (phexByteStr "fe") $ pcon PSNil
```
would yield a `PList PByteString` with one element - `0xfe`. Of course, you could have done that with ``pcons # phexByteStr "fe" # pnil`` instead!

You can also use `pmatch` to match on a list-
```hs
pmatch (pcon $ PSCons (phexByteStr "fe") $ pcon PSNil) $ \case
  PSNil -> "hey hey there's nothing here!"
  PSCons _ _ -> "oooo fancy!"
```
But you should prefer `pelimList` from `PListLike` instead-
```hs
pelimList (\_ _ -> "oooo fancy") "hey hey there's nothing here!" $ pcon $ PSCons (phexByteStr "fe") $ pcon PSNil
```

### PBuiltinPair
Much like in the case of builtin lists, you'll just be working with builtin functions (or rather, Plutarch synonyms to builtin functions) here. You can find everything about that in [builtin-pairs](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-pairs.md). Feel free to only read the `Plutarch` examples.

### PAsData
This is a typed way of representing [`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md). It is highly encouraged you use `PAsData` to keep track of what "species" of `Data` value you actually have. `Data` can be a `Constr` (for sum of products - ADTs), `Map` (for wrapping assoc maps of Data to Data), `List` (for wrapping builtin lists of data), `I` (for wrapping builtin integers), and `B` (for wrapping builtin bytestrings).

> Aside: You might be asking, what's an "`I` data value"? This is referring to the different constructors of `Data`/`BuiltinData`. You can find a full explanation of this at [plutonomicon](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md).

Consider a function that takes in and returns a `B` data value - aka `ByteString` as a `Data` value. If you use the direct Plutarch synonym to `Data` - `PData`, you'd have-
```hs
foo :: Term s (PData :--> PData)
```
That's not very informative - you have no way to ensure that you're actually working with `B` data values. You could use `PAsData` instead-
```hs
foo :: Term s (PAsData PByteString :--> PAsData PByteString)
```
Now, you have assurance that you're working with a `Data` value that actually represents a builtin bytestring!

`PAsData` also makes deconstructing data simple and type safe. When working with raw `PData`, you might find yourself using `pasConstr`, `pasList` etc. on the type `PData`. If `PData` wasn't actually a `Constr` data value - `pasConstr` would cause an evaluation error at runtime.

Instead, you should use `pfromData` (from the typeclass `PIsData`) which will use the correct builtin function depending on the type of `PAsData` you have. Some useful instances of it are-
```hs
pfromData :: Term s (PAsData PInteger) -> Term s PInteger

pfromData :: Term s (PAsData PByteString) -> Term s PByteString

pfromData :: Term s (PAsData (PBuiltinList (PAsData a))) -> Term s (PBuiltinList (PAsData a))
```

You can also create a `PAsData` value from supported types, using `pdata`. Some of the useful instances of it are-
```hs
pdata :: Term s PInteger -> Term s (PAsData PInteger)

pdata :: Term s PByteString -> Term s (PAsData PByteString)

pdata :: Term s (PBuiltinList (PAsData a)) -> Term s (PAsData (PBuiltinList (PAsData a)))
```

In general, if `PIsData T => T` is a Plutarch type (that can be converted to and from `Data`), `PAsData T` is its `Data` representation.

You can also create a `PAsData` from a `PData`, but you lose specific type information along the way-
```hs
pdata :: Term s PData -> Term s (PAsData PData)
```

### PDataSum & PDataList
TODO

See: [`PIsDataRepr`](#pisdatarepr)

### PData
This is a direct synonym to [`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md). As such, it doesn't keep track of what "species" of `Data` it actually is. Is it an `I` data? Is it a `B` data? Nobody can tell for sure!

Consider using [`PAsData`](#pasdata) instead for simple cases, i.e cases other than `Constr`.

Consider using [`PDataSum`/`PDataList`](#PDataSum--pdatalist) instead when dealing with ADTs, i.e `Constr` data values.

You can find more information about `PData` at [Developers' Corner](./DEVGUIDE.md).

### PRecord

You can define and use product ADTs, including records with named fields in Plutarch similar to Haskell's records. For a
Haskell data type like

```hs
data Circle = Circle{
  x, y :: Integer,
  radius :: Natural
  }
```

the equivalent in Plutarch would be

```hs
data Circle f = Circle{
  x, y :: f PInteger,
  radius :: f PNatural
  }
Plutarch.Rec.TH.deriveAll ''Circle
```

Each field type needs to be wrapped into the type parameter `f` of kind `PType -> Type`. This is a slight modification
of a common coding style known as Higher-Kinded Data.

With this definition, `PRecord Circle` will be an instance of [PlutusType](#plutustype-pcon-and-pmatch), so you can use
the usual `pcon` and `pcon'` to construct its value and `pmatch` and `pmatch'` to de-construct it:

```hs
circle :: Term s (PRecord Circle)
circle = pcon $ PRecord Circle{
  x = 100,
  y = 100,
  radius = 50
  }

distanceFromOrigin :: Term s (PRecord Circle :--> PNatural)
distanceFromOrigin = plam $ flip pmatch $ \(PRecord Circle{x, y})-> sqrt #$ projectAbs #$ x * x + y * y
```

You may also find `rcon` and `rmatch` from `Plutarch.Rec` a bit more convenient because they don't require the `PRecord`
wrapper. Alternatively, instead of using `pmatch` or its alternatives you can access individual fields using the `field`
accessor from the same module:

```hs
containsOrigin :: Term s (PRecord Circle :--> PBool)
containsOrigin = plam $ \c-> distanceFromOrigin # c #< pto c # field radius
```

#### letrec

You can use records to define mutually-recursive functions, or more generally (but less usefully) mutually-recursive values.

```hs
circleFixedPoint :: Term s (PRecord Circle)
circleFixedPoint = punsafeFrom $ letrec $ \Circle{y, radius}-> Circle{
  x = y,
  y = 2 * radius,
  radius = 50
  }
```

#### Record Data

You can provide a `PIsData` instance for `PRecord Circle` using the following definition:

```hs
instance RecordFromData Circle
instance PIsData (PRecord Circle) where
  pfromData = readData $ recordFromFieldReaders Circle{
    x = DataReader pfromData,
    y = DataReader pfromData,
    radius = DataReader pfromData
    }
  pdata = writeData $ recordDataFromFieldWriters Circle{
    x = DataWriter pdata,
    y = DataWriter pdata,
    radius = DataWriter pdata
    }
```

If your record has many fields and you only need to a couple of them from `Data`, it's more efficient to use `pfromData`
only on individual fields. You can focus on a single field using the function `fieldFromData`:

```hs
radiusFromCircleData :: Term s (PAsData (PRecord Circle) :--> PAsData PNatural)
radiusFromCircleData = fieldFromData radius
```

# Examples
Be sure to check out [Compiling and Running](#compiling-and-running) first!

## Fibonacci number at given index
```hs
import Plutarch.Prelude

fib :: Term s (PInteger :--> PInteger)
fib = phoistAcyclic $
  pfix #$ plam $ \self n ->
    pif
      (n #== 0)
      0
      $ pif
        (n #== 1)
        1
        $ self # (n - 1) + self # (n - 2)
```
from [examples](../examples).

Execution-
```hs
> evalT $ fib # 2
Right (ExBudget {exBudgetCPU = ExCPU 8289456, exBudgetMemory = ExMemory 19830},[],Program () (Version () 1 0 0) (Constant () (Some (ValueOf integer 2))))
```

## Validator that always succeeds
```hs
import Plutarch.Prelude
import Plutarch.Api.Contexts

alwaysSucceeds :: Term s (PDatum :--> PRedeemer :--> PScriptContext :--> PUnit)
alwaysSucceeds = plam $ \datm redm ctx -> pconstant ()
```
All the arguments are ignored. We use `PData` here for `datm` and `redm` since we're not using them - so we don't need specific type information about them. Any `Data` value is fine.

Execution-
```hs
> alwaysSucceeds `evalWithArgsT` [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData ()]
Right (ExBudget {exBudgetCPU = ExCPU 297830, exBudgetMemory = ExMemory 1100},[],Program () (Version () 1 0 0) (Constant () (Some (ValueOf unit ()))))
```

## Validator that always fails
```hs
import Plutarch.Prelude
import Plutarch.Api.Contexts
import Plutarch.Api.Scripts

alwaysFails :: Term s (PDatum :--> PRedeemer :--> PScriptContext :--> PUnit)
alwaysFails = plam $ \datm redm ctx -> perror
```
Similar to the example above.

Execution-
```hs
> alwaysFails `evalWithArgsT` [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData ()]
Left (EvaluationError [] "(CekEvaluationFailure,Nothing)")
```

## Validator that checks whether a value is present within signatories
```hs
-- NOTE: REQUIRES GHC 9!
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordDotSyntax #-}

import Plutarh.Prelude
import Plutarch.Api.Contexts
import Plutarch.Api.Crypto
import Plutarch.Api.Scripts
import qualified Plutarch.Monadic as P

checkSignatory :: Term s (PPubKeyHash :--> PDatum :--> PRedeemer :--> PScriptContext :--> PUnit)
checkSignatory = plam $ \ph _ _ ctx' -> P.do
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  let
    purpose = pfromData ctx.purpose
    txInfo = pfromData ctx.txInfo
  PSpending _ <- pmatch purpose
  let signatories = pfromData $ pfield @"signatories" # txInfo
  pif (pelem # pdata ph # signatories)
    -- Success!
    (pconstant ())
    -- Signature not present.
    perror
```
> Note: The above snippet usages GHC 9 features (`QualifiedDo` and `RecordDotSyntax`). Be sure to check out [how to translate the do syntax to GHC 8](#translating-do-syntax-to-ghc-8) and [alternatives to `RecordDotSyntax`](TODO: LINK).

Once again, we ignore datum and redeemer so we can use `PData` as typing. Other than that, we match on the script purpose to see if its actually for *spending* - and we get the signatories field from `txInfo` (the 7th field), check if given pub key hash is present within the signatories and that's it!

It's important that we pass a `PPubKeyHash` *prior* to treating `checkSignatory` as a validator script.
```hs
hashStr :: String
hashStr = "abce0f123e"

pubKeyHash :: Term s PPubKeyHash
pubKeyHash = pcon $ PPubKeyHash $ phexByteStr hashStr

mockCtx :: ScriptContext
mockCtx =
  ScriptContext
    (TxInfo
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      (interval (POSIXTime 1) (POSIXTime 2))
      [fromString hashStr, "f013", "ab45"]
      mempty
      ""
    )
    (Spending (TxOutRef "" 1))

> evalWithArgsT (checkSignatory # pubKeyHash) [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData mockCtx]
Right (ExBudget {exBudgetCPU = ExCPU 8969609, exBudgetMemory = ExMemory 17774},[],Program () (Version () 1 0 0) (Constant () (Some (ValueOf unit ()))))
```

## Using custom datum/redeemer in your Validator
TODO

## Manually extracting fields from `ScriptContext` (UNTYPED)
see: [Developers' corner](./DEVGUIDE.md#extracting-txinfoinputs-from-scriptcontext-manually-untyped)

# Thumb rules, Tips, and Tricks

## Plutarch functions are strict

All Plutarch functions are strict. When you apply a Plutarch function to an argument using `#` or `#$` - the argument will be evaluated before being passed into to the function. If you don't want the argument to be evaluated, you can use `pdelay`.

## Don't duplicate work

Consider the simple snippet-

```haskell
pf :: Term s PInteger
pf = let foo = 1 + 2 in pif (foo #== 3) foo 7
```

If you use `printTerm` on this, you'll notice that the computation bound to `foo` is inlined twice-

```
(program 1.0.0 ((\\i0 -> force (i1 (equalsInteger (addInteger 1 2) 3) (delay (addInteger 1 2)) (delay 7))) (force ifThenElse)))
```

Notice how the `addInteger` computation is present _twice_. In these cases, you should use `plet` to compute once and re-use the computed value-

```haskell
pf :: Term s PInteger
pf = plet (1 + 3) $ \foo -> pif (foo #== 3) foo 7
```

Here's another example of this, Haskell level functions-

```haskell
abs :: Term s PInteger -> Term s PInteger
abs x = pif (x #<= -1) (negate x) x
```

`x` is going to be inlined _three_ times there. That's really bad if it's a big computation. This is what I should do instead-

```haskell
abs :: Term s PInteger -> Term s PInteger
abs x' = plet x' $ \x -> pif (x #<= -1) (negate x) x
```

Of course, what you _really_ should do , is prefer Plutarch level functions whenever possible.

## Prefer Plutarch level functions

Plutarch level functions have a lot of advantages - they can be hoisted; they are strict so you can [use their arguments however many times you like without duplicating work](#dont-duplicate-work); they are required for Plutarch level higher order functions etc. Unless you _really_ need laziness, like `pif` does, try to use Plutarch level functions.

Also see: [Hoisting](#hoisting-metaprogramming--and-fundamentals).

## When to use Haskell level functions?
Although you should generally [prefer Plutarch level functions](#prefer-plutarch-level-functions), there are times when a Haskell level function is actually much better. However, figuring out *when* that is the case is a delicate art.

There is one simple and straightforward usecase though, when you want a function argument to be lazily evaluated. In such a case, you should use a Haskell level functions that `pdelay`s the argument before calling some Plutarch level function. Recall that [Plutarch level functions are strict](#plutarch-functions-are-strict).

Outside of that straightforward usecase, figuring out when to use Haskell level functions is quite complex. Haskell level functions will always be inlined when generating the Plutus Core. Unless the function is used *only once*, this sort of inlining will increase the script size - which is problematic.

However, if the function is used *only once*, and making it Plutarch level causes extra `plam`s and `#`s to be introduced - you should just make it Haskell level. For example, consider the `pelimList` implementation-
```hs
pelimList :: PLift a => Term s (a :--> PBuiltinList a :--> r) -> Term s r -> Term s (PBuiltinList a) -> Term s r
pelimList match_cons match_nil ls = pmatch ls $ \case
  PCons x xs -> match_cons # x # xs
  PNil -> match_nil
```
It takes in a Plutarch level function, let's see a typical usage-
```hs
pelimList
  (plam $ \x xs -> pcons # x # (self # xs))
  pnil
  ls
```
This is rather redundant, the above snippet will exhibit inlining to produce-
```hs
pmatch ls $ \case
  PCons x xs -> (plam $ \x xs -> pcons # x # (self # xs)) # x # xs
  PNil -> match_nil
```
Extra `plam`s and `#`s have been introduced. Really, `pelimList` could have taken a Haskell level function instead-
```hs
pelimList :: PLift a => (Term s a -> Term s (PBuiltinList a) :--> Term s r) -> Term s r -> Term s (PBuiltinList a) -> Term s r
pelimList match_cons match_nil ls = pmatch ls $ \case
  PCons x xs -> match_cons x xs
  PNil -> match_nil
```
Now, the following usage-
```hs
pelimList
  (\x xs -> pcons # x # (self # xs))
  pnil
  ls
```
would turn into-
```hs
pmatch ls $ \case
  PCons x xs -> pcons # x # (self # xs)
  PNil -> match_nil
```
It turns out that `pelimList` usages *almost always* use a one-off Haskell level function with a redundant `plam` - so `pelimList` would benefit greatly from just taking a Haskell level function directly.

However, **not all higher order functions** benefit from taking Haskell level functions. In many HOF usages, you could benefit from passing a commonly used function argument, rather than a one-off function argument. Imagine `map`, you don't always map with one-off functions - often, you `map` with existing, commonly used functions. In these cases, that commonly used function ought to be a Plutarch level function, so it can be hoisted and `map` can simply reference it.

## Hoisting is great - but not a silver bullet

Hoisting is only beneficial for sufficiently large lambdas. Hoisting a builtin function, for example - is not very useful-

```haskell
import Plutarch
import qualified PlutusCore as PLC

phoistAcyclic $ punsafeBuiltin PLC.UnListData
```

The term will be the same size anyway. However, if you had a larger term due to, say, using `pforce` -

```haskell
phoistAcyclic $ pforce $ punsafeBuiltin PLC.UnListData
```

Here, hoisting may be beneficial.

You don't need to hoist the top level Plutarch function that you would just pass to `compile`.

## The difference between `PlutusType`/`PCon` and `PLift`'s `pconstant`
`PlutusType` is especially useful for building up Plutarch terms *dynamically* - i.e, from arbitrary Plutarch terms. This is when your Plutarch type's constructors contain other Plutarch terms.

Another case `PlutusType` is useful is when you want to give your Plutarch type a custom representation, scott encoding, enum - what have you. From the `PlutusType` haddock example-
```hs
data AB = A | B

instance PlutusType AB where
  type PInner AB _ = PInteger
  pcon' A = 0
  pcon' B = 1
  pmatch' x f = pif (x #== 0) (f A) (f B)
```
You can use the `A` and `B` constructors during building, but still have your type be represented as integers under the hood! You cannot do this with `pconstant`.

You should prefer `pconstant` (from [`PConstant`/`PLift`](#pconstant--plift)) when you can build something up entirely from Haskell level constants and that *something* has the same representation as the Haskell constant.

# Common Issues

## No instance for (PUnsafeLiftDecl a)
You should add `PLift a` to the context! `PLift` is just a synonym to `PUnsafeLiftDecl`.

## Infinite loop / Infinite AST

While maybe not immediately obvious, things like the following are a no-go in Plutarch:

```haskell
f :: Term s (PInteger :--> PInteger)
f = phoistAcyclic $ plam $ \n ->
  pif (n #== 0)
    0
    $ n + f # (n - 1)
```

The issue here is that the AST is infinitely large. Plutarch will try to traverse this AST and will in the process not terminate, as there is no end to it. In this case you'd fix it by using `pfix`.

Relevant issue: [#19](https://github.com/Plutonomicon/plutarch/issues/19)

## Couldn't match type `Plutarch.DataRepr.Internal.PUnLabel ...` arising from a use of `pfield` (or `hrecField`, or `pletFields`)
You might get some weird errors when using `pfield`/`hrecField`/`pletFields` like the above. Don't be scared! It just means that the type application you used is incorrect. Specifically, the type application names a non-existent field. Re-check the field name string you used in the type application for typos!

## Expected a type, but "fieldName" has kind `GHC.Types.Symbol`
This just means the argument of a type application wasn't correctly promoted. Most likely arising from a usage of `pfield`/`hrecField`/`pletFields`. In the case of `pfield` and `hrecField`, the argument of type application should have kind `Symbol`. A simple string literal representing the field name should work in this case. In the case of `pletFields`, the argument of type application should have kind `[Symbol]` - a type level list of types with kind `Symbol`. When you use a singleton list here, like `["foo"]` - it's actually parsed as a *regular* list (like `[a]`). A regular list, of course, has kind `Type`.

All you need to do, is put a `'` (quote) infront of the list, like so- `@'["foo"]`. This will promote the `[a]` to the type level.

## Lifting `PAsData`
Don't try to lift a `PAsData` term! It's intentionally blocked and partial. The `PLift` instance for `PAsData` is only there to make some important functionality work correctly. But the instance methods will simply error if used. Instead, you should extract the `Term s a` out of `Term s (PAsData a)` using `pfromData` and `plift` that instead!

# Useful Links
- [Plutonomicon](https://github.com/Plutonomicon/plutonomicon)
