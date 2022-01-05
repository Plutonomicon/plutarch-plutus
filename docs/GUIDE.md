
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
  - [Concepts](#concepts)
    - [Hoisting, metaprogramming,  and fundamentals](#hoisting-metaprogramming--and-fundamentals)
    - [What is the `s`?](#what-is-the-s)
    - [eDSL Types in Plutarch](#edsl-types-in-plutarch)
    - [`plet` to avoid work duplication](#plet-to-avoid-work-duplication)
    - [Tracing](#tracing)
    - [Raising errors](#raising-errors)
    - [Delay and Force](#delay-and-force)
  - [Typeclasses](#typeclasses)
    - [Equality and Order](#equality-and-order)
    - [Monoids](#monoids)
    - [PIntegral](#pintegral)
    - [PIsData](#pisdata)
    - [PlutusType, PCon, and PMatch](#plutustype-pcon-and-pmatch)
    - [PIsDataRepr & PDataList](#pisdatarepr--pdatalist)
      - [Implementing PIsDataRepr](#implementing-pisdatarepr)
  - [Working with Types](#working-with-types)
    - [PInteger](#pinteger)
    - [PBool](#pbool)
    - [PString](#pstring)
    - [PByteString](#pbytestring)
    - [PUnit](#punit)
    - [PBuiltinList](#pbuiltinlist)
    - [PBuiltinPair](#pbuiltinpair)
    - [PAsData](#pasdata)
    - [PData](#pdata)
    - [PMaybe](#pmaybe)
    - [PEither](#peither)
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
  - [Hoisting is great - but not a silver bullet](#hoisting-is-great---but-not-a-silver-bullet)
- [Common Issues](#common-issues)
  - [`plam` fails to type infer correctly](#plam-fails-to-type-infer-correctly)
  - [Infinite loop / Infinite AST](#infinite-loop--infinite-ast)
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

## Syntax

A Plutarch script is a `Term`. This can consist of-

### Constants

These are either built using `pcon` (for types that have a `PlutusType` or `PCon` instance)-

```haskell
import Plutarch.Prelude
import Plutarch.Bool (PBool (PTrue))

-- | A plutarch level boolean. Its value is "True", in this case.
x :: Term s PBool
x = pcon PTrue
-- Note that 'PBool' has a 'PlutusType' instance.
```

Or by using literals-

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Plutarch.Prelude
import Plutarch.Integer (PInteger)
import Plutarch.String (PString)

-- | A plutarch level integer. Its value is 1, in this case.
x :: Term s PInteger
x = 1

-- | A plutarch level string (this is actually 'Text'). Its value is "foobar", in this case.
y :: Term s PString
y = "foobar"
```

Or by using other constructor functions provided by Plutarch-

```haskell
import qualified Data.ByteString as BS
import Plutarch.Prelude
import Plutarch.ByteString (PByteString, phexByteStr, pbyteStr)

-- | A plutarch level bytestring. Its value is [65], in this case.
x :: Term s PByteString
x = phexByteStr "41"
-- ^ 'phexByteStr' interprets a hex string as a bytestring. 0x41 is 65 - of course.

-- | A plutarch level bytestring. Its value is [65], in this case.
y :: Term s PByteString
y = pbyteStr $ BS.pack [65]
-- ^ pbyteStr lifts a Haskell level bytestring into Plutarch.
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
pif (pcon PTrue) 1 2
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
import Plutarch.Integer (PInteger)

pfac :: Term s (PInteger :--> PInteger)
pfac = pfix #$ plam f
  where
    f :: Term s (PInteger :--> PInteger) -> Term s PInteger -> Term s PInteger
    f self n = pif (n #== 1) n $ n * (self #$ n - 1)
-- (ignore the existence of non positives :D)
```

There's a Plutarch level factorial function! Note how `f` takes in a `self` and just recurses on it. All you have to do, is create a Plutarch level function by using `plam` on `f` and `pfix` the result - and that `self` argument will be taken care of for you.

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

Any use of `x` will inline the **full definition** of `x`. `x + x` will duplicate `something complex` in the AST. To avoid this, you should use `plet` in order to avoid duplicate work. Do note that this is **strictly evaluated, and hence isn't always the best solution.**

There is however still a problem: What about top-level functions, like `fib`, `sum`, `filter`, and such? We can use `plet` to avoid duplicating the definition, but this error-prone, since to do this perfectly each function that generates part of the AST would need to have access to the `plet`'ed definitions, meaning that we'd likely have to put it into a record or typeclass.

To solve this problem, Plutarch supports _hoisting_. Hoisting only works for _closed terms_, that is, terms that don't reference any free variables (introduced by `plam`).

Hoisted terms are essentially moved to a top-level `plet`, i.e. it's essentially common subexpression elimination. Do note that because of this, your hoisted term is **also strictly evaluated, meaning that you shouldn't hoist non-lazy complex computations (use e.g.** `pdelay` **to avoid this).**

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
You can use the functions `ptrace`, `ptraceError`, `ptraceIfFalse`, `ptraceIfTrue` (from `Plutarch.Trace`) for tracing. These behave similarly to the ones you're used to from [PlutusTx](https://staging.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Trace.html).

If you have the `development` flag for `plutarch` turned on - you'll see the trace messages appear in the trace log during script evaluation. When not in development mode - these functions basically do nothing.

### Raising errors
In Plutus Tx, you'd signal validation failure with the [`error`](https://staging.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Prelude.html#v:error) function. You can do the same in Plutarch using `perror`.
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
where `6` and `3` are `Term s PInteger`s yields `3` - also a `Term s PInteger`.

### PIsData
The `PIsData` typeclass facilitates easy and safe conversion between `PAsData` and types that can represent `PData` - i.e [`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md).
```hs
class PIsData a where
  pfromData :: Term s (PAsData a) -> Term s a
  pdata :: Term s a -> Term s (PAsData a)
```

`PInteger` has a `PIsData` instance-
```hs
instance PIsData PInteger where
  pfromData x = pasInt # pforgetData x
  pdata x = punsafeBuiltin PLC.IData # x
```
In essence, `pdata` wraps a `PInteger` into an `I` data value. Wheras `pfromData` simply unwraps the `I` data value to get a `PInteger`.

> Aside: When implementing `PIsData` for your type, `a` - you can safely use `punsafeCoerce` for both `pdata` and `pfromData` if **you're sure** that `a` is actually a `Data` value under the hood (e.x `PData`).

### PlutusType, PCon, and PMatch
`PlutusType` lets you construct and deconstruct Plutus Core constants from Haskell ADTs. It's essentially a combination of `PCon` (for constant construction) and `PMatch` (for constant deconstruction).

```hs
class (PCon a, PMatch a) => PlutusType (a :: k -> Type) where
  type PInner a (b' :: k -> Type) :: k -> Type
  pcon' :: forall s. a s -> forall b. Term s (PInner a b)
  pmatch' :: forall s c. (forall b. Term s (PInner a b)) -> (a s -> Term s c) -> Term s c
```
`PInner` is meant to represent the "inner" type of `a` - the Plutarch type representing the Plutus Core constant used to represent `a`.

Here's the `PlutusType` instance for `PBool`-
```hs
data PBool s = PTrue | PFalse

instance PlutusType PBool where
  type PInner PBool _ = PBool
  pcon' PTrue = punsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniBool True
  pcon' PFalse = punsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniBool False
  pmatch' b f = pforce $ pif' # b # pdelay (f PTrue) # pdelay (f PFalse)
```
There's a lot of jazz in here - but in essence, booleans are represented as Plutus Core booleans in that `pcon'` and they're deconstructed using `pif'` - which is just a synonym to the builtin function, `IfThenElse`.

You should always use `pcon` and `pmatch` instead of `pcon'` and `pmatch'` - these are provided by the `PCon` and `PMatch` typeclasses-
```hs
class PCon a where
  pcon :: a s -> Term s a

class PMatch a where
  pmatch :: Term s a -> (a s -> Term s b) -> Term s b
```

All `PlutusType` instances get `PCon` and `PMatch` instances for free!

For types that cannot easily be both `PCon` and `PMatch` - feel free to implement just one of them! However, in general, **prefer implementing PlutusType**!

### PIsDataRepr & PDataList
`PIsDataRepr` and `PDataList` are the user-facing parts of an absolute workhorse of a machinery for easily deconstructing `Constr` [`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md) values. It allows fully type safe matching on `Data` values, without embedding type information within the generated script - unlike PlutusTx.

For example, `PScriptContext` - which is the Plutarch synonym to [`ScriptContext`](https://staging.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Contexts.html#t:ScriptContext) - has a `PIsDataRepr` instance, this lets you easily keep track of its type and match on it-
```hs
import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.ScriptContext

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

We are interested in the second field, `PScriptPurpose`, synonymous to [`ScriptPurpose`](https://staging.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Contexts.html#t:ScriptPurpose). So we do `pdhead #$ pdtail # te`. This gives us a `PAsData PScriptPurpose`. Finally a `pfromData` will get you the `PScriptPurpose` directly. This is because `PScriptPurpose` has a `PIsData` instance! It is a `Data` value under the hood, after all.

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

This is synonymous to Plutus Core [builtin integer](https://staging.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html#t:Integer).

### PBool

Plutarch level boolean terms can be constructed using `pcon Ptrue` and `pcon PFalse`. `PBool` itself is just `data PBool = PFalse | PTrue`. It has a `PlutusType` instance, allowing you to use `pcon` to construct Plutarch terms using Haskell constructors.

```haskell
pif (pcon PFalse) 7 42
-- evaluates to 42
```
You can combine Plutarch booleans terms using `#&&` and `#||`, which are synonyms to `&&` and `||`. These are haskell level operators and therefore have short circuiting. If you don't need short circuiting, you can use the Plutarch level alternatives- `pand'` and `por'` respectively.

This is synonymous to Plutus Core [builtin boolean](https://staging.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins-Internal.html#t:BuiltinBool).

### PString

`Term s PString` has a `IsString` instance. This allows you to make Plutarch level string terms from regular string literals, provided you have `OverloadedStrings` turned on.

```haskell
{-# LANGUAGE OverloadedStrings #-}

"foo"
```

where &quot;foo&quot; is actually `Term s PString`.

It also has a `PEq` instance. And its terms have  `Semigroup` and `Monoid` instances - which work the way you would expect.

It **does not** have a `PlutusType` instance.

This is synonymous to Plutus Core [builtin string](https://staging.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html#t:BuiltinString) (actually Text).

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

This is synonymous to Plutus Core [builtin bytestring](https://staging.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html#t:BuiltinByteString).

### PUnit

The Plutarch level unit term can be constructed using `pcon PUnit`.

This is synonymous to Plutus Core [builtin unit](https://staging.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins-Internal.html#t:BuiltinUnit).

### PBuiltinList
You'll be using builtin lists quite a lot in Plutarch. Plutarch comes with utilities and synonyms to make this easier.

Generally, you'll just be working with builtin functions (or rather, Plutarch synonyms to builtin functions) while working with lists. You can find everything about that in [builtin-lists](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-lists.md). Feel free to only read the `Plutarch` examples.

### PBuiltinPair
Much like in the case of builtin lists, you'll just be working with builtin functions (or rather, Plutarch synonyms to builtin functions) here. You can find everything about that in [builtin-pairs](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-pairs.md). Feel free to only read the `Plutarch` examples.

### PAsData
This is a typed way of representing [`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md). It is highly encouraged you use `PAsData` to keep track of what "species" of `Data` value you actually have. `Data` can be a `Constr` (for sum of products - ADTs), `Map` (for wrapping assoc maps of Data to Data), `List` (for wrapping builtin lists of data), `I` (for wrapping builtin integers), and `B` (for wrapping builtin bytestrings).

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

You can also create a `PAsData` from a `PData`, but you lose specific type information along the way-
```hs
pdata :: Term s PData -> Term s (PAsData PData)
```

### PData
This is a direct synonym to [`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md). As such, it doesn't keep track of what "species" of `Data` it actually is. Is it an `I` data? Is it a `B` data? Nobody can tell for sure!

Consider using `PAsData` instead for simple cases, i.e cases other than `Constr`.

Consider using `PDataRepr` instead when dealing with ADTs, i.e `Constr` data values.

You can find more information about `PData` at [Developers' Corner](https://mlabs.slab.com/posts/plutarch-xlifp008#hvezd-developers-corner).

### PMaybe
TODO

### PEither
TODO

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
from [examples](./examples/Main.hs).

Execution-
```hs
> evalT $ fib # 2
Right (ExBudget {exBudgetCPU = ExCPU 8289456, exBudgetMemory = ExMemory 19830},[],Program () (Version () 1 0 0) (Constant () (Some (ValueOf integer 2))))
```

## Validator that always succeeds
```hs
import Plutarch.Prelude
import Plutarch.Unit
import Plutarch.ScriptContext

alwaysSucceeds :: Term s (PData :--> PData :--> PScriptContext :--> PUnit)
alwaysSucceeds = plam $ \datm redm ctx -> pcon PUnit
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
import Plutarch.Unit
import Plutarch.ScriptContext

alwaysFails :: Term s (PData :--> PData :--> PScriptContext :--> PUnit)
alwaysFails = plam $ \datm redm ctx -> perror
```
Similar to the example above.

Execution-
```hs
> alwaysFails `evalWithArgsT` [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData ()]
Left (EvaluationError [] "(CekEvaluationFailure,Nothing)")
```

## Validator that checks whether a value is present within signatories
TODO

## Using custom datum/redeemer in your Validator
TODO

## Manually extracting fields from `ScriptContext` (UNTYPED)
see: [Developers' corner](#extracting-txinfoinputs-from-scriptcontext-manually-untyped)

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

Plutarch level functions have a lot of advantages - they can be hoisted; they are strict so you can [use their arguments however many times you like without duplicating work](https://mlabs.slab.com/posts/plutarch-xlifp008#hoy4m-dont-duplicate-work); they are required for Plutarch level higher order functions etc. Unless you _really_ need laziness, like `pif` does, try to use Plutarch level functions.

What about convenient Haskell operators? Well, these must be Haskell level functions working on Plutarch terms. This is the case for `+`, `-`, `#==` and many more.

Choosing convenience over efficiency is difficult, but if you notice that your operator uses complex logic and may end up creating big terms, like in this case-

```haskell
(#||) :: Term s PBool -> Term s PBool -> Term s PBool
x #|| y = pif x (pcon PTrue) $ pif y (pcon PTrue) $ pcon PFalse
```

You can factor out most of the logic to a Plutarch level function, and apply that in the operator definition-

```haskell
(#||) :: Term s PBool -> Term s PBool -> Term s PBool
x #|| y = por # pdelay x # pdelay y

por :: Term s (PDelayed PBool :--> PDelayed PBool :--> PBool)
por = phoistAcyclic $
  plam $
    \x y -> pif' # pforce x # pcon PTrue #$ pif' # pforce y # pcon PTrue # pcon PFalse
```

The necessity of workarounds like these will be significantly reduced once we figure out [eta reductions (#32)](https://github.com/Plutonomicon/plutarch/issues/32) though!

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

# Common Issues

## `plam` fails to type infer correctly

This is a known issue, see: [#2](https://github.com/Plutonomicon/plutarch/issues/2)

Sometimes, GHC will not be able to infer the type of an argument within a lambda you pass to `plam`. This happens most often when the argument is unused.

Giving unused arguments the type `_ :: Term _ _` should fix the issue generally.

Because of this, you might want to enable  `PartialTypeSignatures`.

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

# Useful Links
- [Plutonomicon](https://github.com/Plutonomicon/plutonomicon)
