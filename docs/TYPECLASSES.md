This document describes the primary typeclasses used in Plutarch.

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

<details>
<summary> Table of Contents </summary>

-   [`PEq` & `PORd`](#peq--pord)
-   [`PIntegral`](#pintegral)
-   [`PIsData`](#pisdata)
-   [`PConstant` & `PLift`](#pconstant--plift)
    -   [Implementing `PConstant` & `PLift`](#implementing-pconstant--plift)
-   [`PlutusType`, `PCon`, and `PMatch`](#plutustype-pcon-and-pmatch)
    -   [Implementing `PlutusType` for your own types (Scott Encoding)](#implementing-plutustype-for-your-own-types-scott-encoding)
    -   [Implementing `PlutusType` for your own types (`Data` Encoding)](#implementing-plutustype-for-your-own-types-data-encoding)
    -   [Implementing `PlutusType` for your own types (`newtype`)](#implementing-plutustype-for-your-own-types-newtype)
-   [`PListLike`](#plistlike)
-   [`PIsDataRepr` & `PDataFields`](#pisdatarepr--pdatafields)
    -   [All about extracting fields](#all-about-extracting-fields)
        -   [Alternatives to `OverloadedRecordDot`](#alternatives-to-overloadedrecorddot)
    -   [All about constructing data values](#all-about-constructing-data-values)
    -   [Implementing `PIsDataRepr` and friends](#implementing-pisdatarepr-and-friends)

</details>

# `PEq` & `PORd`

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

> Jack: missing full stop.

There is also a synonym to  `Ord`, `POrd`-

> Jack: synonym _of_

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

# `PIntegral`

This is similar to the `Integral` typeclass. However, it only has the following class methods-

-   `pdiv` - similar to `div`
-   `pmod` - similar to `mod`
-   `pquot` - similar to `quot`
-   `prem` - similar to `rem`

Using these functions, you can do division/modulus etc on Plutarch level values-

> Jack: etc. \[with a '.']

```hs
pdiv # 6 # 3
```

where `6` and `3` are `Term s PInteger`s yields `2` - also a `Term s PInteger`.

# `PIsData`

The `PIsData` typeclass facilitates easy and type safe conversion between Plutarch types and their corresponding [`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md) representation. It keeps track of the type information through [`PAsData`](./TYPES.md#pasdata).

> Jack: i.e. \[with two full stops]. You've done this in a number of places.

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

> Jack: Whereas

> Aside: You might be asking, what's an "`I` data value"? This is referring to the different constructors of `Data`/`BuiltinData`. You can find a full explanation of this at [plutonomicon](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md).

> Jack: Plutonomicon is a proper noun and must be capitalized.

For the simple constructors that merely wrap a builtin type into `Data`, e.g integers, bytestrings, lists, and map, `PIsData` works in much the same way as above. However, what about `Constr` data values? When you have an ADT that doesn't correspond to those simple builtin types directly - but you still need to encode it as `Data` (e.g `PScriptContext`). In this case, you should [implement `PIsDataRepr`](#implementing-pisdatarepr-and-friends) and you'll get the `PIsData` instance for free!

> Jack: Consider 'built-in'.

# `PConstant` & `PLift`

These 2 closely tied together typeclasses establish a bridge between a Plutarch level type (that is represented as a builtin type, i.e [`DefaultUni`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-core/html/PlutusCore.html#t:DefaultUni)) and its corresponding Haskell synonym. The gory details of these two are not too useful to users, but you can read all about it if you want at [Developers' corner](./DEVGUIDE.md#pconstant-and-plift).

> Jack: These two closely...

What's more important, are the abilities that `PConstant`/`PLift` instances have-

```hs
pconstant :: PLift p => PLifted p -> Term s p

plift :: (PLift p, HasCallStack) => ClosedTerm p -> PLifted p
```

> Aside: `PLifted p` represents the Haskell synonym to the Plutarch type, `p`. Similarly, there is also `PConstanted h` - which represents the Plutarch synonym corresponding to the Haskell type, `h`.

`pconstant` lets you build a Plutarch value from its corresponding Haskell synonym. For example, the haskell synonym of [`PBool`](./TYPES.md#pbool) is [`Bool`](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Bool.html#t:Bool).

> Jack: capitalize Haskell.

```hs
b :: Term s PBool
b = pconstant False
```

On the other end, `plift` lets you obtain the Haskell synonym of a Plutarch value (that is represented as a builtin value, i.e [`DefaultUni`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-core/html/PlutusCore.html#t:DefaultUni))-

```hs
import Plutus.V1.Ledger.Contexts

purp :: Term s PScriptPurpose
purp = pconstant $ Minting "be"

> plift purp
Minting "be"
```

There's also another handy utility, `pconstantData`-

```hs
pconstantData :: (PLift p, ToData (PLifted p)) => PLifted p -> Term s (PAsData p)
```

> Note: This isn't the actual type of `pconstantData` - it's simplified here for the sake of documentation ;)

It's simply the `PAsData` building cousin of `pconstant`!

## Implementing `PConstant` & `PLift`

If your custom Plutarch type is represented by a builtin type under the hood (i.e not [scott encoded](CONCEPTS.md#scott-encoding) - rather one of the [`DefaultUni`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-core/html/PlutusCore.html#t:DefaultUni) types) - you can implement `PLift` for it by using the provided machinery.

> Jack: capitalize Scott.

This comes in 3 flavors.

> Jack: three flavors.

-   Plutarch type represented **directly** by a builtin type that **is not** `Data` (`DefaultUniData`) ==> `DerivePConstantDirect`

    Ex: `PInteger` is directly represented as a builtin integer.
-   Plutarch type represented **indirectly** by a builtin type that **is not** `Data` (`DefaultUniData`) ==> `DerivePConstantViaNewtype`

    Ex: `PPubKeyHash` is a newtype to a `PByteString`, and `PByteString` is _directly_ represented as a builtin bytestring.
-   Plutarch type represented by `Data`, i.e. [data encoded](./CONCEPTS.md#data-encoding)(`DefaultUniData`) ==> `DerivePConstantViaData`

    Ex: `PScriptPurpose` is represented as a `Data` value. It is synonymous to `ScriptPurpose` from the Plutus ledger api.

Whichever path you need to go down, there is one common part- implementing `PLift`, or rather `PUnsafeLiftDecl`. See, `PLift` is actually just a type synonym to `PUnsafeLiftDecl`. Essentially an empty typeclass with an associated type family that provides insight on the relationship between a Plutarch type and its Haskell synonym.

```hs
instance PUnsafeLiftDecl YourPlutarchType where
  type PLifted YourPlutarchType = YourHaskellType
```

You're tasked with assigning the correct Haskell synonym to your Plutarch type, and what an important task it is! Recall that `pconstant`'s argument type will depend on your assignment here. In particular: `pconstant :: YourHaskellType -> YourPlutarchType`.

Some examples:-

-   for `YourPlutarchType` = `PInteger`, `YourHaskellType` = `Integer`

    ```hs
    instance PUnsafeLiftDecl PInteger where type PLifted PInteger = Integer
    ```
-   for `YourPlutarchType` = `PValidatorHash`, `YourHaskellType` = `ValidatorHash`

    ```hs
    instance PUnsafeLiftDecl PValidatorHash where type PLifted PValidatorHash = Plutus.ValidatorHash
    ```
-   for `YourPlutarchType` = `PScriptPurpose`, `YourHaskellType` = `ScriptPurpose`

    ```hs
    instance PUnsafeLiftDecl PScriptPurpose where type PLifted PScriptPurpose = Plutus.ScriptPurpose
    ```

Now, let's get to implementing `PConstant` for the Haskell synonym, via the 3 methods. The first of which is `DerivePConstantDirect`-

> Jack: three methods.

```hs
{-# LANGUAGE UndecidableInstances #-}

import Plutarch.Lift (DerivePConstantDirect (DerivePConstantDirect))
import Plutarch.Prelude

deriving via (DerivePConstantDirect Integer PInteger) instance (PConstant Integer)
```

`DerivePConstantDirect` takes in 2 type parameters-

> Jack: two type parameters. N.B. In prose, numbers less than 10 should be written as a word.

-   The Haskell type itself, for which `PConstant` is being implemented for.
-   The **direct** Plutarch synonym to the Haskell type.

Pretty simple! Let's check out `DerivePConstantViaNewtype` now-

```hs
{-# LANGUAGE UndecidableInstances #-}

import Plutarch.Lift (DerivePConstantViaNewtype (DerivePConstantViaNewtype))
import Plutarch.Prelude

import qualified Plutus.V1.Ledger.Api as Plutus

newtype PValidatorHash (s :: S) = PValidatorHash (Term s PByteString)

...

deriving via (DerivePConstantViaNewtype Plutus.ValidatorHash PValidatorHash PByteString) instance (PConstant Plutus.ValidatorHash)
```

`DerivePConstantViaNewtype` takes in 3 type parameters-

-   The Haskell newtype itself, for which `PConstant` is being implemented for.
-   The Plutarch synonym to the Haskell type.
-   The actual Plutarch type corresponding to the Haskell type contained within the newtype.

    E.g `ValidatorHash` is a newtype to a `ByteString`, which is synonymous to `PByteString`. In the same way, `PValidatorHash` is actually just a newtype to a `PByteString` term.
    During runtime, `ValidatorHash` is actually just a `ByteString`, the same applies for `PValidatorHash`. So we give it the `newtype` treatment with `DerivePConstantViaNewtype`!

Finally, we have `DerivePConstantViaData` for `Data` values-

```hs
{-# LANGUAGE UndecidableInstances #-}

import Plutarch.Lift (DerivePConstantViaNewtype (DerivePConstantViaNewtype))
import Plutarch.Prelude

import qualified Plutus.V1.Ledger.Api as Plutus

data PScriptPurpose (s :: S)
  = PMinting (Term s (PDataRecord '["_0" ':= PCurrencySymbol]))
  | PSpending (Term s (PDataRecord '["_0" ':= PTxOutRef]))
  | PRewarding (Term s (PDataRecord '["_0" ':= PStakingCredential]))
  | PCertifying (Term s (PDataRecord '["_0" ':= PDCert]))

...

deriving via (DerivePConstantViaData Plutus.ScriptPurpose PScriptPurpose) instance (PConstant Plutus.ScriptPurpose)
```

`DerivePConstantViaData` takes in 2 type parameters-

-   The Haskell type itself, for which `PConstant` is being implemented for.
-   The Plutarch synonym to the Haskell type.
    And that's all you need to know to implement `PConstant` and `PLift`!

# `PlutusType`, `PCon`, and `PMatch`

`PlutusType` is the primary typeclass that determines the underlying representation for a Plutarch type. It lets you construct and deconstruct Plutus Core constants from from a Plutarch type's constructors (possibly containing other Plutarch terms). It's essentially a combination of `PCon` (for term construction) and `PMatch` (for term deconstruction).

> Jack: from from

```hs
class (PCon a, PMatch a) => PlutusType (a :: k -> Type) where
  type PInner a (b' :: k -> Type) :: k -> Type
  pcon' :: forall s. a s -> forall b. Term s (PInner a b)
  pmatch' :: forall s c. (forall b. Term s (PInner a b)) -> (a s -> Term s c) -> Term s c
```

> Note: You don't need to look too much into the types! After all, you'll be using `pcon` and `pmatch`, rather than `pcon'` and `pmatch'`.
> `PInner` is meant to represent the "inner" type of `a` - the Plutarch type representing the Plutus Core constant used to represent `a`.

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

This is a [scott encoded representation of the familiar `Maybe` data type](./CONCEPTS.md#scott-encoding). As you can see, `PInner` of `PMaybe` is actually a Plutarch level function. And that's exactly why `pcon'` creates a _function_. `pmatch'`, then, simply "matches" on the function - scott encoding fashion.

> Jack: Scott

You should always use `pcon` and `pmatch` instead of `pcon'` and `pmatch'` - these are provided by the `PCon` and `PMatch` typeclasses-

```hs
class PCon a where
  pcon :: a s -> Term s a

class PMatch a where
  pmatch :: Term s a -> (a s -> Term s b) -> Term s b
```

All `PlutusType` instances get `PCon` and `PMatch` instances for free!

For types that cannot easily be both `PCon` and `PMatch` - feel free to implement just one of them! However, in general, **prefer implementing `PlutusType`**!

## Implementing `PlutusType` for your own types (Scott Encoding)

If you want to represent your data type with [scott encoding](./CONCEPTS.md#scott-encoding) (and therefore don't need to make it `Data` encoded), you should simply derive it generically-

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

## Implementing `PlutusType` for your own types (`Data` Encoding)

If your type is supposed to be represented using [`Data` encoding](./CONCEPTS.md#data-encoding-and-scott-encoding) instead (i.e has a [`PIsDataRepr`](#pisdatarepr--pdatafields) instance), you can derive `PlutusType` via `PIsDataReprInstances`-

=======
## Implementing `PlutusType` for your own types (`Data` Encoding)
If your type is supposed to be represented using [`Data` encoding](./CONCEPTS.md#data-encoding) instead (i.e has a [`PIsDataRepr`](#pisdatarepr--pdatafields) instance), you can derive `PlutusType` via `PIsDataReprInstances`-

```hs
import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch.Prelude
import Plutarch.DataRepr (PIsDataReprInstances(PIsDataReprInstances))

data MyType (a :: PType) (b :: PType) (s :: S)
  = One (Term s (PDataRecord '[ "_0" ':= a ]))
  | Two (Term s (PDataRecord '[ "_0" ':= b ]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances (MyType a b)
```

See: [Implementing `PIsDataRepr` and friends](#implementing-pisdatarepr-and-friends)

> Jack: full stop required.

## Implementing `PlutusType` for your own types (`newtype`)
See: [`DerivePNewtype`](./USAGE.md#deriving-typeclasses-for-newtypes)

# `PListLike`

The `PListLike` typeclass bestows beautiful, and familiar, list utilities to its instances. Plutarch has two list types- [`PBuiltinList`](./TYPES.md#pbuiltinlist) and [`PList`](./TYPES.md#plist). Both have `PListLike` instances! However, `PBuiltinList` can only contain builtin types. It cannot contain Plutarch functions. The element type of `PBuiltinList` can be constrained using `PLift a => PBuiltinList a`.

> Jack: beautiful and familiar

> Note: `PLift` is exported from `Plutarch.Lift`.

As long as it's a `PLift a => PBuiltinList a` or `PList a` - it has access to all the `PListLike` goodies, out of the box. It helps to look into some of these functions at [`Plutarch.List`](https://github.com/Plutonomicon/plutarch/blob/master/Plutarch/List.hs).

Along the way, you might be confronted by 2 big mean baddies ...err, constraints-

```hs
PIsListLike list a
```

This just means that the type `list a`, is _indeed_ a valid `PListLike` containing valid elements! Of course, all `PList a`s are valid `PListLike`, but we have to think about `PBuiltinList` since it can only contain `PLift a => a` elements! So, in essence a function declared as-

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

# `PIsDataRepr` & `PDataFields`

`PIsDataRepr` allows for easily constructing _and_ deconstructing `Constr` [`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md) values. It allows fully type safe matching on [`Data` encoded](./CONCEPTS.md#data-encoding) values, without embedding type information within the generated script - unlike PlutusTx. `PDataFields`, on top of that, allows for ergonomic field access.

> Aside: What's a `Constr` data value? Briefly, it's how Plutus Core encodes non-trivial ADTs into `Data`/`BuiltinData`. It's essentially a sum-of-products encoding. But you don't have to care too much about any of this. Essentially, whenever you have a custom non-trivial ADT (that isn't just an integer, bytestring, string/text, list, or assoc map) - and you want to represent it as a data encoded value - you should implement `PIsDataRepr` for it.

For example, `PScriptContext` - which is the Plutarch synonym to [`ScriptContext`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Contexts.html#t:ScriptContext) - has the necessary instances. This lets you easily keep track of its type, match on it, deconstruct it - you name it!

```hs
import Plutarch.Prelude
import Plutarch.Api.V1.Contexts

pmatchC :: PlutusType a => Term s a -> TermCont s (a s)
pmatchC = tcont . pmatch

foo :: Term s (PScriptContext :--> PString)
foo = plam $ \ctx -> unTermCont $ do
  purpose <- pmatchC $ pfield @"purpose" # ctx
  pure $ case purpose of
    PMinting _ -> "It's minting!"
    PSpending _ -> "It's spending!"
    PRewarding _ -> "It's rewarding!"
    PCertifying _ -> "It's certifying!"
```

Of course, just like `ScriptContext` - `PScriptContext` is represented as a `Data` value in Plutus Core. Plutarch just lets you keep track of the _exact representation_ of it within the type system.

Here's how `PScriptContext` is defined-

```hs
newtype PScriptContext (s :: S)
  = PScriptContext
      ( Term
          s
          ( PDataRecor, numbers less than 10 should be written as a word.
              '[ "txInfo" ':= PTxInfo
               , "purpose" ':= PScriptPurpose
               ]
          )
      )
```

It's a constructor containing a [`PDataRecord`](./TYPES.md#pdatasum--pdatarecord) term. It has 2 fields- `txInfo` and `purpose`.

First, we extract the `purpose` field using `pfield @"purpose"`-

```hs
pfield :: Term s (PScriptContext :--> PScriptPurpose)
```

> Note: When extracting several fields from the same variable, you should instead use `pletFields`. See: [Extracting fields](#all-about-extracting-fields)

> Aside: `pfield` is actually return type polymorhpic. It could've returned either `PAsData PScriptPurpose` and `PScriptPurpose`. In this case, GHC correctly infers that we actually want a `PScriptPurpose`, since `pmatch` doesn't work on `PAsData PScriptPurpose`!
>
> Sometimes GHC isn't so smart, and you're forced to provide an explicit type annotation. Or you can simply use `pfromData $ pfield ....`.

Now, we can `pmatch` on our `Term s PScriptPurpose` to extract the Haskell ADT (`PScriptPurpose s`) out of the Plutarch term-

```hs
pmatch :: Term s PScriptPurpose -> (PScriptPurpose s -> Term s PString) -> Term s PString
```

Now that we have `PScriptPurpose s`, we can just `case` match on it! `PScriptPurpose` is defined as-

```hs
data PScriptPurpose (s :: S)
  = PMinting (Term s (PDataRecord '["_0" ':= PCurrencySymbol]))
  | PSpending (Term s (PDataRecord '["_0" ':= PTxOutRef]))
  | PRewarding (Term s (PDataRecord '["_0" ':= PStakingCredential]))
  | PCertifying (Term s (PDataRecord '["_0" ':= PDCert]))
```

It's just a Plutarch sum type.

We're not really interested in the fields (the `PDataRecord` term), so we just match on the constructor with the familar `case`. Easy!

Let's pass in a `ScriptContext` as a `Data` value from Haskell to this Plutarch script and see if it works!

```hs
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Interval
import qualified PlutusTx

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
Right (Program () (Version () 1 0 0) (Constant () (Some (ValueOf string "It's minting!"))))
```

> Aside: You can find the definition of `evalWithArgsT` at [Compiling and Running](./GUIDE.md#compiling-and-running).

## All about extracting fields

We caught a glimpse of field extraction in the example above, thanks to `pfield`. However, that barely touched the surface.

Once a type has a `PDataFields` instance, field extraction can be done with these 3 functions-

-   `pletFields`
-   `pfield`
-   `hrecField` (when not using `OverloadedRecordDot` or record dot preprocessor)

> Jack: consider linking to record dot preprocessor.

Each has its own purpose. However, `pletFields` is arguably the most general purpose and most efficient. Whenever you need to extract several fields from the same variable, you should use `pletFields`-

```hs
-- NOTE: REQUIRES GHC 9!
{-# LANGUAGE OverloadedRecordDot #-}

import Plutarch.Prelude
import Plutarch.Api.V1.Contexts

foo :: Term s (PScriptContext :--> PUnit)
foo = plam $ \ctx' -> do
  ctx <- tcont $ pletFields @["txInfo", "purpose"] ctx'
  let
    purpose = ctx.purpose
    txInfo = ctx.txInfo
  -- <use purpose and txInfo here>
  pconstant ()
```

> Note: The above snippet uses GHC 9 features (`OverloadedRecordDot`). Be sure to check out [alternatives to `OverloadedRecordDot`](#alternatives-to-overloadedrecorddot).

In essence, `pletFields` takes in a type level list of the field names that you want to access and a continuation function that takes in an `HRec`. This `HRec` is essentially a collection of the bound fields. You don't have to worry too much about the details of `HRec`. This particular usage has type-

```hs
pletFields :: Term s PScriptContext
  -> (HRec
        (BoundTerms
           '[ "txInfo" ':= PTxInfo, "purpose" ':= PScriptPurpose]
           '[ 'Bind, 'Bind]
           s)
      -> Term s PUnit)
  -> Term s PUnit
```

You can then access the fields on this `HRec` using `OverloadedRecordDot`.

Next up is `pfield`. You should _only ever_ use this if you just want one field from a variable and no more. It's usage is simply `pfield @"fieldName" # variable`. You can, however, also use `pletFields` in this case (e.g `pletFoelds @'["fieldName"] variable`). `pletFields` with a singular field has the same efficiency as `pfield`!

> Jack: Its usage

Finally, `hrecField` is merely there to supplement the lack of record dot syntax. See: [Alternative to `OverloadedRecordDot`](#alternative-to-overloadedrecorddot).

> Note: An important thing to realize that `pfield` and `hrecField` (or overloaded record dot on `HRec`) are _return type polymorphic_. They can return both `PAsData Foo` or `Foo` terms, depending on the surrounding context. This is very useful in the case of `pmatch`, as `pmatch` doesn't work on `PAsData` terms. So you can simply write `pmatch $ pfield ...` and `pfield` will correctly choose to _unwrap_ the `PAsData` term.

> Jack: realize is that

### Alternatives to `OverloadedRecordDot`

If `OverloadedRecordDot` is not available, you can also try using the [record dot preprocessor plugin](https://hackage.haskell.org/package/record-dot-preprocessor).

If you don't want to use either, you can simply use `hrecField`. In fact, `ctx.purpose` above just translates to `hrecField @"purpose" ctx`. Nothing magical there!

## All about constructing data values

We learned about type safe matching (through `PlutusType`) as well as type safe field access (through `PDataFields`) - how about construction? Since `PIsDataRepr` allows you to derive [`PlutusType`](#plutustype-pcon-and-pmatch), and `PlutusType` bestows the ability to not only _deconstruct_, but also **construct** values - you can do that just as easily!

Let's see how we could build a `PMinting` `PScriptPurpose` given a `PCurrencySymbol`-

```hs
import Plutarch.Prelude
import Plutarch.Api.V1

currSym :: Term s PCurrencySymbol
```

```hs
purpose :: Term s PScriptPurpose
purpose = pcon $ PMinting fields
  where
    currSymDat :: Term _ (PAsData PCurrencySymbol)
    currSymDat = pdata currSym
    fields :: Term _ (PDataRecord '[ "_0" ':= PCurrencySymbol ])
    fields = pdcons # currSymDat # pdnil
```

All the type annotations are here to help!

This is just like regular `pcon` usage you've [seen above](#plutustype-pcon-and-pmatch). It takes in the Haskell ADT of your Plutarch type and gives back a Plutarch term.

What's more interesting, is the `fields` binding. Recall that `PMinting` is a constructor with one argument, that argument is a [`PDataRecord`](./TYPES.md#pdatasum--pdatarecord) term. In particular, we want: `Term s (PDataRecord '["_0" ':= PCurrencySymbol ])`. It encodes the exact type, position, and name of the field. So, all we have to do is create a `PDataRecord` term!

Of course, we do that using `pdcons` - which is just the familiar `cons` specialized for `PDataRecord` terms.

```hs
pdcons :: forall label a l s. Term s (PAsData a :--> PDataRecord l :--> PDataRecord ((label ':= a) ': l))
```

It takes a `PAsData a` and adds that `a` to the `PDataRecord` heterogenous list. We feed it a `PAsData PCurrencySymbol` term and `pdnil` - the empty data record. That should give us-

```hs
pdcons # currSymDat # pdnil :: Term _ (PDataRecord '[ label ':= PCurrencySymbol ])
```

Cool! Wait, what's `label`? It's the field name associated with the field, in our case, we want the field name to be `_0` - because that's what the `PMinting` constructor wants. You can either specify the label with a type application or you can just have a type annotation for the binding (which is what we do here). Or you can let GHC try and match up the `label` with the surrounding environment!

Now that we have `fields`, we can use it with `PMinting` to build a `PScriptPurpose s` and feed it to `pcon` - we're done!

## Implementing `PIsDataRepr` and friends

Implementing these is rather simple with generic deriving + `PIsDataReprInstances`. All you need is a well formed type using `PDataRecord`. For example, suppose you wanted to implement `PIsDataRepr` for the Plutarch version of this Haskell type-

> Jack: avoid '+' in prose.

```hs
data Vehicle
  = FourWheeler Integer Integer Integer Integer
  | TwoWheeler Integer Integer
  | ImmovableBox
```

You'd declare the corresponding Plutarch type as-

```hs
import Plutarch.Prelude

data PVehicle (s :: S)
  = PFourWheeler (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PInteger, "_2" ':= PInteger, "_3" ':= PInteger]))
  | PTwoWheeler (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PInteger]))
  | PImmovableBox (Term s (PDataRecord '[]))
```

> Note: The constructor ordering in `PVehicle` matters! If you used [`makeIsDataIndexed`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html#v:makeIsDataIndexed) on `Vehicle` to assign an index to each constructor - the Plutarch type's constructors must follow the same indexing order.
>
> In this case, `PFourWheeler` is at the 0th index, `PTwoWheeler` is at the 1st index, and `PImmovableBox` is at the 3rd index. Thus, the corresponding `makeIsDataIndexed` usage should be-
>
> ```hs
> PlutusTx.makeIsDataIndexed ''FourWheeler [('FourWheeler,0),('TwoWheeler,1),('ImmovableBox,2)]
> ```
>
> Also see: [Isomorphism between Haskell ADTs and `PIsDataRepr`](./TRICKS.md#the-isomorphism-between-makeisdataindexed-haskell-adts-and-pisdatarepr)

And you'd simply derive `PIsDataRepr` using generics. However, you **must** also derive `PIsData` using `PIsDataReprInstances`. Moreover, you should also derive `PlutusType`. For single constructor data types, you should also derive `PDataFields`.

Combine all that, and you have-

```hs
{-# LANGUAGE UndecidableInstances #-}

import qualified GHC.Generics as GHC
import Generics.SOP

import Plutarch.Prelude
import Plutarch.DataRepr (PIsDataReprInstances (PIsDataReprInstances))

data PVehicle (s :: S)
  = PFourWheeler (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PInteger, "_2" ':= PInteger, "_3" ':= PInteger]))
  | PTwoWheeler (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PInteger]))
  | PImmovableBox (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PVehicle
```

> Note: You cannot derive `PIsDataRepr` for types that are represented using [scott encoding](./CONCEPTS.md#scott-encoding). Your types must be well formed and should be using `PDataRecord` terms instead.

That's it! Now you can represent `PVehicle` as a `Data` value, as well as deconstruct and access its fields super ergonomically. Let's try it!

```hs
-- NOTE: REQUIRES GHC 9!
{-# LANGUAGE OverloadedRecordDot #-}

import Plutarch.Prelude

pmatchC :: PlutusType a => Term s a -> TermCont s (a s)
pmatchC = tcont . pmatch

test :: Term s (PVehicle :--> PInteger)
test = plam $ \veh' -> unTermCont $ do
  veh <- pmatchC veh'
  case veh of
    PFourWheeler fwh' -> do
      fwh <- tcont $ pletFields @'["_0", "_1", "_2", "_3"] fwh'
      pure $ fwh._0 + fwh._1 + fwh._2 + fwh._3
    PTwoWheeler twh' -> P.do
      twh <- tcont $ pletFields @'["_0", "_1"] twh'
      pure $ twh._0 + twh._1
    PImmovableBox _ -> pure 0
```

> Note: The above snippet uses GHC 9 features (`OverloadedRecordDot`). Be sure to check out [alternatives to `OverloadedRecordDot`](#alternatives-to-overloadedrecorddot).

What about types with singular constructors? It's quite similar to the sum type case. Here's how it looks-

```hs
{-# LANGUAGE UndecidableInstances #-}

import qualified GHC.Generics as GHC
import Generics.SOP

import Plutarch.Prelude
import Plutarch.DataRepr (PIsDataReprInstances (PIsDataReprInstances))

newtype PFoo (s :: S) = PMkFoo (Term s (PDataRecord '["foo" ':= PByteString]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PFoo
```

Just an extra `PDataFields` derivation compared to the sum type usage!
