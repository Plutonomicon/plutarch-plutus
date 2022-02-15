This document discusses thumb rules and general trivia, aiming to make life as a Plutarch user or auditor easier.

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

<details>
<summary> Table of Contents </summary>

- [Plutarch functions are strict](#plutarch-functions-are-strict)
- [Don't duplicate work](#dont-duplicate-work)
  - [Where should arguments be `plet`ed?](#where-should-arguments-be-pleted)
- [Prefer Plutarch level functions](#prefer-plutarch-level-functions)
- [When to use Haskell level functions?](#when-to-use-haskell-level-functions)
- [Hoisting is great - but not a silver bullet](#hoisting-is-great---but-not-a-silver-bullet)
- [The difference between `PlutusType`/`PCon` and `PLift`'s `pconstant`](#the-difference-between-plutustypepcon-and-plifts-pconstant)
- [List iteration is strict](#list-iteration-is-strict)
- [Let Haskell level functions take responsibility of evaluation](#let-haskell-level-functions-take-responsibility-of-evaluation)
- [The isomorphism between `makeIsDataIndexed`, Haskell ADTs, and `PIsDataRepr`](#the-isomorphism-between-makeisdataindexed-haskell-adts-and-pisdatarepr)
- [Prefer statically building constants whenever possible](#prefer-statically-building-constants-whenever-possible)
- [Figuring out the representation of a Plutarch type](#figuring-out-the-representation-of-a-plutarch-type)

</details>

# Plutarch functions are strict

All Plutarch functions are strict. When you apply a Plutarch function to an argument using `papp` (or `#`/`#$` - synonyms to `papp`) - the argument will be evaluated before being passed into to the function. If you don't want the argument to be evaluated, you can use `pdelay`.

# Don't duplicate work

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

`x` is going to be inlined _three_ times there. That's really bad if it's a big computation. This is what you should do instead-

```haskell
abs :: Term s PInteger -> Term s PInteger
abs x' = plet x' $ \x -> pif (x #<= -1) (negate x) x
```

Of course, what you _really_ should do , is prefer Plutarch level functions whenever possible.

## Where should arguments be `plet`ed?
You don't have to worry about work duplication on arguments in *every single scenario*. In particular, the argument to `plam` is also a Haskell function, isn't it? But you don't need to worry about `plet`ing your arguments there since it becomes a Plutarch level function through `plam` - thus, all the arguments are evaluated before being passed in.

Where else is `plet` unnecessary? Functions taking in continuations, such as `plet` (duh) and `pletFields`, always pre-evaluate the binding. An exception, however, is `pmatch`. In certain cases, you don't need to `plet` bindings within the `pmatch` case handler. For example, if you use `pmatch` on a `PList`, the `x` and `xs` in the `PSCons x xs` *will always be pre-evaluated*. On the other hand, if you use `pmatch` on a `PBuiltinList`, the `x` and `xs` in the `PCons x xs` *are **not** pre-evaluated*. Be sure to `plet` them if you use them several times!

In general, `plet`ing something back to back several times will be optimized to a singular `plet` anyway. However, you should know that for data encoded types (types that follow "[implementing `PIsDataRepr` and friends](./TYPECLASSES.md#implementing-pisdatarepr-and-friends)") and scott encoded types, `pmatch` handlers get pre-evaluated bindings. For `PBuiltinList`, and `PDataRecord` - the bindings are not pre-evaluated.

You should also `plet` local bindings! In particular, if you applied a function (whether it be Plutarch level or Haskell level) to obtain a value, bound the value to a variable (using `let` or `where`) - don't use it multiple times! The binding will simply get inlined as the function application - and it'll keep getting re-evaluated. You should `plet` it first!

This also applies to field accesses using `OverloadedRecordDot`. When you do `ctx.purpose`, it really gets translated to `hrecField @"purpose" ctx` - that's a function call! If you use the field multiple times, `plet` it first.

# Prefer Plutarch level functions

Plutarch level functions have a lot of advantages - they can be hoisted; they are strict so you can [use their arguments however many times you like without duplicating work](#dont-duplicate-work); they are required for Plutarch level higher order functions etc. Unless you _really_ need laziness, like `pif` does, try to use Plutarch level functions.

Also see: [Hoisting](./CONCEPTS.md#hoisting-metaprogramming--and-fundamentals).

# When to use Haskell level functions?
Although you should generally [prefer Plutarch level functions](#prefer-plutarch-level-functions), there are times when a Haskell level function is actually much better. However, figuring out *when* that is the case - is a delicate art.

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

# Hoisting is great - but not a silver bullet

Hoisting is only beneficial for sufficiently large terms. Hoisting a builtin function, for example - is not very useful-

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

# The difference between `PlutusType`/`PCon` and `PLift`'s `pconstant`
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

You should prefer `pconstant` (from [`PConstant`/`PLift`](./TYPECLASSES.md#pconstant--plift)) when you can build something up entirely from Haskell level constants and that *something* has the same representation as the Haskell constant.

# List iteration is strict
Chained list operations (e.g a filter followed by a map) are not very efficient in Plutus Core. In fact, the iteration is not lazy at all! For example, if you did a `pfilter`, followed by a `pmap`, on a builtin list - the entire `pmap` operation would be computed first, the whole list would be iterated through, and *only then* the `pfilter` would start computing. Ridiculous!

# Let Haskell level functions take responsibility of evaluation
We've discussed how a Haskell level function that operates on Plutarch level terms needs to [be careful](#dont-duplicate-work) about [work duplication](./CONCEPTS.md#plet-to-avoid-work-duplication). Related to this point, it's good practice to design your Haskell level functions so that *it takes responsibility* for evaluation.

The user of your Haskell level function doesn't know how many times it uses the argument it has been passed! If it uses the argument multiple times without `plet`ing it - there's duplicate work! There's 2 solutions to this-
* The user `plet`s the argument before passing it to the Haskell level function.
* The Haskell level function takes responsibility of its argument and `plet`s it itself.

The former is problematic since it's based on *assumption*. What if the Haskell level function is a good rule follower, and correctly `plet`s its argument if using it multiple times? Well, then there's a `plet` by the caller *and* the callee. It won't evaluate the computation twice, so that's good! But it does increase the execution units and the script size a bit!

Instead, try to offload the responsbility of evaluation to the Haskell level function - so that it only `plet`s when it needs to.

Of course, this is not applicable for recursive Haskell level functions!

# The isomorphism between `makeIsDataIndexed`, Haskell ADTs, and `PIsDataRepr`
When [implementing `PIsDataRepr`](#implementing-pisdatarepr-and-friends) for a Plutarch type, if the Plutarch type also has a Haskell synonym (e.g `ScriptContext` is the haskell synonym to `PScriptContext`) that uses [`makeIsDataIndexed`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html#v:makeIsDataIndexed) - you must make sure the constructor ordering is correct.

> Aside: What's a "Haskell synonym"? It's simply the Haskell type that *is supposed to* correspond to a Plutarch type. There doesn't *necessarily* have to be some sort of concrete connection (though there can be, using [`PLift`/`PConstant`](./TYPECLASSES.md#pconstant--plift)) - it's merely a connection you can establish mentally.
>
> This detail does come into play in concrete use cases though. After compiling your Plutarch code to a `Script`, when you pass Haskell data types as arguments to the `Script` - they obviously need to correspond to the actual arguments of the Plutarch code. For example, if the Plutarch code is a function taking `PScriptContext`, after compilation to `Script`, you *should* pass in the Haskell data type that actually shares the same representation as `PScriptContext` - the "Haskell synonym", so to speak. In this case, that's `ScriptContext`.

In particular, with `makeIsDataIndexed`, you can assign *indices* to your Haskell ADT's constructors. This determines how the ADT will be represented in Plutus Core. It's important to ensure that the corresponding Plutarch type *knows* about these indices so it can decode the ADT correctly - in case you passed it into Plutarch code, through Haskell.

For example, consider `Maybe`. Plutus assigns these indices to its constructors-
```hs
makeIsDataIndexed ''Maybe [('Just, 0), ('Nothing, 1)]
```
0 to `Just`, 1 to `Nothing`. So the corresponding Plutarch type, `PMaybeData` is defined as-
```hs
data PMaybeData a (s :: S)
  = PDJust (Term s (PDataRecord '["_0" ':= a]))
  | PDNothing (Term s (PDataRecord '[]))
```
It'd be a very subtle mistake to instead define it as-
```hs
data PMaybeData a (s :: S)
  = PDNothing (Term s (PDataRecord '[]))
  | PDJust (Term s (PDataRecord '["_0" ':= a]))
```
The constructor ordering is wrong!

It's not just constructor ordering that matters - field ordering does too! Though this is self explanatory. Notice how `PTxInfo` shares the exact same field ordering as its Haskell synonym - `TxInfo`.
```hs
newtype PTxInfo (s :: S)
  = PTxInfo
      ( Term
          s
          ( PDataRecord
              '[ "inputs" ':= PBuiltinList (PAsData PTxInInfo)
               , "outputs" ':= PBuiltinList (PAsData PTxOut)
               , "fee" ':= PValue
               , "mint" ':= PValue
               , "dcert" ':= PBuiltinList (PAsData PDCert)
               , "wdrl" ':= PBuiltinList (PAsData (PTuple PStakingCredential PInteger))
               , "validRange" ':= PPOSIXTimeRange
               , "signatories" ':= PBuiltinList (PAsData PPubKeyHash)
               , "data" ':= PBuiltinList (PAsData (PTuple PDatumHash PDatum))
               , "id" ':= PTxId
               ]
          )
      )
```
```hs
data TxInfo = TxInfo
  { txInfoInputs      :: [TxInInfo]
  , txInfoOutputs     :: [TxOut]
  , txInfoFee         :: Value
  , txInfoMint        :: Value
  , txInfoDCert       :: [DCert]
  , txInfoWdrl        :: [(StakingCredential, Integer)]
  , txInfoValidRange  :: POSIXTimeRange
  , txInfoSignatories :: [PubKeyHash]
  , txInfoData        :: [(DatumHash, Datum)]
  , txInfoId          :: TxId
  }
```
The *field names* don't matter though. They are merely labels that don't exist in runtime.

# Prefer statically building constants whenever possible
Whenever you can build a Plutarch constant out of a pure Haskell value - do it! Functions such as `pconstant`, `phexByteStr` operate on regular Haskell synonyms of Plutarch types. Unlike `pcon`, which potentially work on Plutarch terms (ex: `pcon $ PJust x`, `x` is a `Term s a`). A Plutarch term is an entirely "runtime" concept. "Runtime" as in "Plutus Core Runtime". They only get evaluated during runtime!

On the other hand, whenever you transform a Haskell synonym to its corresponding Plutarch type using `pconstant`, `phexByteStr` etc. - you're *directly* building a Plutus Core constant. This is entirely static! There are no runtime function calls, no runtime building, it's just *there*, inside the compiled script.

Here's an example, let's say you want to build a `PScriptPurpose` - `PMinting "f1e301"`. Which snippet, do you think, is better?
```hs
import Plutarch.Prelude
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1.Value

import Plutus.V1.Ledger.Api

pconstant (Minting "f1e301")
-- (or)
let currSym = pcon $ PCurrencySymbol $ phexByteStr "f1e301"
 in pcon $ PMinting $ pdcons # pdata currSym # pdnil
```
The semantics are both are the same. But the former (`pconstant`) compiles to a constant term directly. Whereas the latter compiles to some code that *builds* the constant during Plutus Core runtime.
> Aside: Remember that Haskell runtime is actually compile-time for Plutarch! Even if you have a dynamically computed variable in the Haskell world, it's still a *constant* in the Plutarch world. So you can use it just as well as an argument to `pconstant`!

Whenever you need to build a Plutarch term of type `a`, from a Haskell value, use `pconstant`. Whenever you need to build a Plutarch term of type `PAsData a`, use `pconstantData`!

# Figuring out the representation of a Plutarch type
As discussed in other sections of guide, Plutarch types are merely tags to underlying semantic representations. This is an eDSL after all! Their data declarations *actually* don't matter as far as internal semantics are concerned. It's actually the `PlutusType` instance that *really* determines the representation. So how do you figure out the representation of this seemingly transient tag? By following *conventions*.

You *could* give your Plutarch type a representation that makes no sense given its `data` type declaration, but don't! Instead, most data type declarations follow certain rules to hint at their representations. The representation can only be one of two categories: builtin and scott encoded. All *trivial* builtin types are already defined in Plutarch: `PInteger`, `PByteString`, `PString`, `PBool`, `PUnit`, `PBuiltinList`, and `PBuiltinPair`.

Now, let's discuss patterns of data declarations and what representation they *should* hint at:
* If it's a newtype to a term containing Plutarch type - it should have the same representation as that underlying Plutarch type.

  e.g. `newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)` is just represented as `PByteString`. This is ensured by deriving all necessary instances (particularly `PlutusType`) using [`DerivePNewtype`](./USAGE.md#deriving-typeclasses-for-newtypes).

* If it's an ADT that derives `PlutusType` generically (i.e. `derive anyclass (PlutusType)`)- it uses scott encoding. This is typically the encoding you want for non-trivial data types that don't need to be part of datums or redeemers.

  e.g. `PList` derives `PlutusType` generically and is represented with Scott encoding.

* If it's an ADT that derives `PIsDataRepr` generically (i.e `derive anyclass (PIsDataRepr)`), as well as `PlutusType` via `PIsDataReprInstances`, it's data encoded. Particularly, it's a [`Data`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html#t:Data) value - which is part of the builtin types.

  e.g. `PScriptContext` derives `PIsDataRepr` generically and `PlutusType` via `PIsDataReprInstances`.
