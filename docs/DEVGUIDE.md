Looking to contribute to Plutarch? Looking for functionalities that are not currently provided by Plutarch from a safe interface? You've come to the right place!

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch developers!

<details>
<summary> Table of Contents </summary>

- [Code Style](#code-style)
- [Pre-commit checks](#pre-commit-checks)
- [Updating Changelog](#updating-changelog)
- [Targeting branch for PR](#targeting-branch-for-pr)
- [Concepts](#concepts)
  - [Plutus Core constants (UNSAFE)](#plutus-core-constants-unsafe)
  - [Plutus core builtin functions](#plutus-core-builtin-functions)
  - [Working with BuiltinData/Data/PData](#working-with-builtindatadatapdata)
  - [`PConstant` and `PLift`](#pconstant-and-plift)
- [Lower Level Examples](#lower-level-examples)
  - [Extracting `txInfoInputs` from `ScriptContext` manually (UNTYPED)](#extracting-txinfoinputs-from-scriptcontext-manually-untyped)
- [Useful Links](#useful-links)
- [How to build docs](#how-to-build-docs)

</details>

# Code Style

You should generally follow the [MLabs style guide](https://github.com/mlabs-haskell/styleguide), credit to [@Koz Ross](https://github.com/kozross).

**Discouraged Extensions**

- `ImportQualifiedPost`
- `RecordWildCards`

# Pre-commit checks

Remember to run `./bin/format` to format your code and `cabal test`, alongside `cabal test -f development`, to make sure all the tests pass prior to making a PR!

# Updating Changelog

If your PR makes a change to some user facing functionality - please summarize the change(s) and add it to `CHANGELOG.md`.

# Targeting branch for PR

More often than not, you'll be making PRs directly to `master`.

However, sometimes, there is a release cycle going on and the state of the repository is in flux. There will usually be a `master <- staging` PR open during this time. As long as the `staging` PR is open, you should base most new branches on top of it and merge back into it. Bug fixes, for bugs present in `master`, are exempt from this requirement.

# Concepts

Even if certain functionalities are absent from the public facing API - you can always implement them using functions like `punsafeConstant` and `punsafeBuiltin` - these allow you to walk the lines between Plutus core and Plutarch.

A general familiarity with Plutus core is important. You can learn all of that through the following documents:

- [Builtin lists](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-lists.md)
- [Builtin pairs](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-pairs.md)
- [Builtin functions](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-functions.md)
- [Builtin data](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md)

Parts of the [Pluto guide](https://github.com/Plutonomicon/pluto/blob/main/GUIDE.md) may also prove useful.

## Plutus Core constants (UNSAFE)

> **NOTE**: The following information is almost never necessary with the existence of `pconstant`. Refer to [constant building](./Introduction/Plutarch%20Terms/Plutarch%20Constants.md) and [`PConstant`/`PLift`](./Typeclasses/PConstant%20and%20PLift.md) section of the Plutarch user guide.

Often, you will need to build a Plutus core constant. You can do this using `Some` and `ValueOf`. Here's how `pcon PTrue` creates a Plutarch term that actually evaluates to a Plutus core constant representing a boolean:

```haskell
import qualified PlutusCore as PLC

pcon' PTrue = punsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniBool True
pcon' PFalse = punsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniBool False
```

There's a lot to unpack here - but the general pattern is always the same. First step is to construct the Plutus core constant:

```haskell
PLC.Some $ PLC.ValueOf PLC.DefaultUniBool True
```

The only parts that you will need to change when creating other constants, are the type and the value. Here the type is `DefaultUniBool`. This means the next argument must be a `Bool`. Ensured by the type system - don't you worry :)

You can glance at the other types in the default universe (what you will be working with). Can you guess how to make a Plutus core string from a Haskell string, and represent it as a Plutarch term?

```haskell
import qualified Data.Text as Txt
import qualified PlutusCore as PLC

punsafeConstant . PLC.Some . PLC.ValueOf PLC.DefaultUniString . Txt.pack
```

(it's even pointfree!)

And that's _essentially_ what the `IsString` implementation of `Term s PString` does. That is how your string literals end up as plutus core built in strings.

One more, how about something complex - `DefaultUniProtoList`. This is a builtin list. But what is the element type? Well, you'll have to specify that yourself! You use `DefaultUniApply` to "apply" a type (from the default universe) over `DefaultUniProtoList`:

```haskell
import qualified PlutusCore as PLC

PLC.Some . PLC.ValueOf (PLC.DefaultUniProtoList `PLC.DefaultUniApply` PLC.DefaultUniInteger)
```

That right there converts a `[Integer]` into a Plutus core builtin list of builtin integers. Convenient!

Actually, there's a convenient `pattern` synonym for ``DefaultUniProtoList `DefaultUniApply` a``- `DefaultUniList a`. Using that, you can simplify the above to:

```haskell
PLC.Some . PLC.ValueOf (PLC.DefaultUniList PLC.DefaultUniInteger)
```

Note that you will have to provide the correct type annotations yourself, as `punsafeConstant` just infers to a `Term s a`. That's why it's unsafe! Make sure to provide the correct annotations when using this unsafe function:

```haskell
foo :: Bool -> Term s PBool
foo = punsafeConstant . PLC.Some . PLC.ValueOf PLC.DefaultUniBool
```

Of course, we represent Plutus core booleans as `Term s PBool` in Plutarch - so that's its type!

## Plutus core builtin functions

This is what you will be wrangling with the most. Builtin functions are going to be the foundation of _everything_ you do. And the documentation on them is….. sparse.

You create Plutarch synonyms to Plutus core builtin functions using `punsafeBuiltin`. It creates a Plutarch level function from a Plutus core builtin functions.

Let's try making one, how about `AddInteger`?

```haskell
import qualified PlutusCore as PLC

addI :: Term s (PInteger :--> PInteger :--> PInteger)
addI = punsafeBuiltin PLC.AddInteger
```

Just like `punsafeConstant`, you have to provide the right annotation yourself. We know that `AddInteger` takes two Plutus core builtin integers and returns another one. We represent these integers in Plutarch using `PInteger` terms - so there we go!

You can use and apply this Plutarch function just like any other.

Now here's where this goes off the rails, some builtin functions require _forces_ to be used. These builtin functions have inherent polymorphic type variables. The number of times you need to force them, depends on the number of type variables they have.

Let's look at an example- `HeadList`. It's type can be thought of as - `forall a. [a] -> a`. It has one type variable, so it needs to be forced once:

```haskell
pheadBuiltin :: Term s (PBuiltinList a :--> a)
pheadBuiltin = pforce $ punsafeBuiltin PLC.HeadList
```

We force a Plutarch term using `pforce`, recall that `punsafeBuiltin` returns a term. You need to type it all yourself of course. `pforce` doesn't mean you need to get rid of the type variable in your Plutarch level type. It'll still work with any `a` - the forcing just has to happen at call site.

You can sort of do this blindly, `HeadList` takes 1 force, so just `pforce` once. `TailList` also takes 1 force. `ChooseList` takes 2 forces (`forall a b. [a] -> b -> b -> b`). Here's how you would implement a Plutarch synonym for it:

```haskell
pchooseList :: Term s (PBuiltinList a :--> b -> b -> b)
pchooseList = pforce $ pforce $ punsafeBuiltin PLC.ChooseList
```

> Aside: You should also hoist the synonyms here that take one or more forces!

We have a [Plutus Core builtin functions reference](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-functions.md) for everything you need to know about them. Including types, usage, and forcing.

## Working with BuiltinData/Data/PData

Most of the time, you'll be working with `BuiltinData`/`Data` - this is the type of the arguments that will be passed onto your script from the outside. This is the type of the datum, the redeemer and the script context. This is also the type of arguments you will be able to pass to a `Script`.

Plutarch aims to hide these low level details from the user. Ideally, you will be using `PDataSum`/`PDataList` and `PAsData` - these are essentially just `BuiltinData`, but it is typed at the Plutarch level.

If you want to work with `BuiltinData` directly however, which you may have to do during developing Plutarch, you can find all that you need to know at [Plutonomicon](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md).

## `PConstant` and `PLift`

TODO

# Lower Level Examples

## Extracting `txInfoInputs` from `ScriptContext` manually (UNTYPED)

Here's a quick refresher on what `ScriptContext` looks like:

```haskell
data ScriptContext = ScriptContext
  { scriptContextTxInfo :: TxInfo
  , scriptContextPurpose :: ScriptPurpose
  }
```

We are interested in `txInfoInputs`, which has type `TxInInfo`. It is the first field within `TxInfo`. If you have read [Working with `BuiltinData`](#working-with-builtindatadatapdata) already - you know that a `ScriptContext` translates to a `Data` value similar to:

```haskell
Constr 0 [PlutusTx.toData txInfo, PlutusTx.toData txPurpose]
```

Where `txInfo` and `txPurpose` are values of type `TxInfo` and `ScriptPurpose` respectively.

We are interested in that first field. That's easy, we do the following actions in sequence:

- `pasConstr` - yields a `PBuiltinPair PInteger (PBuiltinList PData)`. We know the constructor id is `0`. It doesn't matter, there's only one constructor.
- `psndBuiltin` - yields `PBuiltinList PData`, the second element of the pair. These are the fields within `ScriptContext`.
- `phead` - yields `PData`, the first field. We know this is our `TxInfo`.

Combining that all up would give you:

```haskell
import Plutarch.Prelude
import Plutarch.Builtin

f :: Term s (PData :--> PData)
f = plam $ \x -> phead #$ psndBuiltin #$ pasConstr # x
```

And if you test it with a mock context value, it does work:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Interval
import qualified PlutusTx

mockCtx :: ScriptContext
mockCtx =
  ScriptContext
    (TxInfo
      [ TxInInfo
          (TxOutRef "" 1)
          (TxOut (Address (PubKeyCredential "0123") Nothing) mempty Nothing)
      ]
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

>  f `evalWithArgsT` [PlutusTx.toData mockCtx]
Right (Program () (Version () 1 0 0) (Constant () (Some (ValueOf data (Constr 0 [List [Constr 0 [Constr 0 [Constr 0 [B ""],I 1],Constr 0 [Constr 0 [Constr 0 [B "\SOH#"],Constr 1 []],Map [],Constr 1 []]]],List [],Map [],Map [],List [],List [],Constr 0 [Constr 0 [Constr 1 [I 1],Constr 1 []],Constr 0 [Constr 1 [I 2],Constr 1 []]],List [],List [],Constr 0 [B ""]])))))
```

> Aside: You can find the definition of `evalWithArgsT` above - [Compiling and Running](./README.md#compiling-and-running).

But we're not done yet! We want `txInfoInputs`. You may have noticed where exactly it is located on the above output. See that `List …`? Inside the outermost `Constr`'s fields? That's our `txInfoInputs`!

> Aside: Recall that `List` data values are simply wrappers around lists. Also recall that the fields in a `Constr` value must be all of type `Data`. So any of your list fields get translated to `List` data. Just remember not to confuse these with builtin lists (`PBuiltinList`)! Functions like `pheadBuiltin` don't work on `List` data values.

To obtain `txInfoInputs` from here, we do the following actions in sequence:

- `pasConstr` - unpacks the `TxInfo`. There's only one constructor, `TxInfo` - we don't care about that. We need the fields.
- `psndBuiltin` - extracts the second member of the pair, the fields of `TxInfo`.
- `phead` - extracts the first element of the list. This is our field, `txInfoInputs`.
- (optional) `pasList` - takes out the builtin list from the `List` data value.

And that's it! Putting it all together:

```haskell
f :: Term s (PData :--> PBuiltinList PData)
f = plam $ \x ->
  let txInfo = phead #$ psndBuiltin #$ pasConstr # x
  in pasList #$ phead #$ psndBuiltin #$ pasConstr # txInfo
```

Trying it on the same `mockCtx` yields:

```haskell
> f `evalWithArgsT` [PlutusTx.toData mockCtx]
Right (Program () (Version () 1 0 0) (Constant () (Some (ValueOf list (data) [Constr 0 [Constr 0 [Constr 0 [B ""],I 1],Constr 0 [Constr 0 [Constr 0 [B "\SOH#"],Constr 1 []],Map [],Constr 1 []]]]))))
```

Getting some of the boilerplate out of the way, this is what the value looks like:

```haskell
Some
  (ValueOf list (data)
    [Constr 0
        [Constr 0 [Constr 0 [B ""],I 1],Constr 0 [Constr 0 [Constr 0 [B "\SOH#"],Constr 1 []],Map [],Constr 1 []]]
    ]
  )
```

There's just one element in `txInfoInputs` in this example, and there it is. Of course `TxInInfo`, the element type of this list, also gets translated to a `Constr` data with further fields. And that's what you see above.

# Useful Links

- [Builtin lists](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-lists.md)
- [Builtin pairs](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-pairs.md)
- [Builtin functions](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-functions.md)
- [Builtin data](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md)
- [Plutus builtin functions and types](https://playground.plutus.iohkdev.io/doc/haddock//plutus-tx/html/PlutusTx-Builtins-Internal.html)
- [Plutus Core builtin function identifiers, aka `DefaultFun`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-core/html/PlutusCore.html#t:DefaultFun)
- [Plutus Core types, aka `DefaultUni`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-core/html/PlutusCore.html#t:DefaultUni)

# How to build docs

To run the docs locally from the Git working copy (useful when editing them),

```sh-session
nix run .#docs
```

To build the static HTML site,

```sh-session
nix build .#website
```

To run the docs directly without cloning the Git repo,

```sh-session
nix run github:Plutonomicon/plutarch#website
```
