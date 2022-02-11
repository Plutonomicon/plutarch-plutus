This document describes the primary typeclasses used in Plutarch.

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

<details>
<summary> Table of Contents </summary>

- [Equality and Order](#equality-and-order)
- [Monoids](#monoids)
- [PIntegral](#pintegral)
- [PIsData](#pisdata)
- [PConstant & PLift](#pconstant--plift)
  - [Implementing `PConstant` & `PLift`](#implementing-pconstant--plift)
- [PlutusType, PCon, and PMatch](#plutustype-pcon-and-pmatch)
  - [Implementing `PlutusType` for your own types (Scott Encoding)](#implementing-plutustype-for-your-own-types-scott-encoding)
  - [Implementing `PlutusType` for your own types (Data Encoding)](#implementing-plutustype-for-your-own-types-data-encoding)
- [PListLike](#plistlike)
- [PIsDataRepr & PDataFields](#pisdatarepr--pdatafields)
  - [All about extracting fields](#all-about-extracting-fields)
    - [Alternatives to `OverloadedRecordDot`](#alternatives-to-overloadedrecorddot)
  - [All about constructing data values](#all-about-constructing-data-values)
  - [Implementing PIsDataRepr and friends](#implementing-pisdatarepr-and-friends)

</details>

# Equality and Order

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

# Monoids

You can use `<>` on two `Term s a`s to produce one `Term s a`, where `Term s a` is a `Semigroup`. You can use `mempty` to create a `Term s a`, where `Term s a` is a monoid.

It works the way you would expect, `PByteString` and `PString` terms have `Monoid` instances-

```haskell
{-# LANGUAGE OverloadedStrings #-}

"ab" <> "cd"
-- evaluates to "abcd"
```

Where all those strings are actually `Term s PString`s.

# PIntegral
This is similar to the `Integral` typeclass. However, it only has the following class methods-
* `pdiv` - similar to `div`
* `pmod` - similar to `mod`
* `pquot` - similar to `quot`
* `prem` - similar to `rem`

Using these functions, you can do division/modulus etc on Plutarch level values-
```hs
pdiv # 6 # 3
```
where `6` and `3` are `Term s PInteger`s yields `2` - also a `Term s PInteger`.

# PIsData
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

For the simple constructors that merely wrap a builtin type into `Data`, e.g integers, bytestrings, lists, and map, `PIsData` works in much the same way as above. However, what about `Constr` data values? When you have an ADT that doesn't correspond to those simple builtin types directly - but you still need to encode it as `Data` (e.g `PScriptContext`). In this case, you should [implement `PIsDataRepr`](#implementing-pisdatarepr-and-friends) and you'll get the `PIsData` instance for free!

# PConstant & PLift
These 2 closely tied together typeclasses establish a bridge between a Plutarch level type (that is represented as a builtin type, i.e [`DefaultUni`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-core/html/PlutusCore.html#t:DefaultUni)) and its corresponding Haskell synonym. The gory details of these two are not too useful to users, but you can read all about it if you want at [Developers' corner](./DEVGUIDE.md#pconstant-and-plift).

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

There's also another handy utility, `pconstantData`-
```hs
pconstantData :: (PLift p, ToData (PLifted p)) => PLifted p -> Term s (PAsData p)
```
> Note: This isn't the actual type of `pconstantData` - it's simplified here for the sake of documentation ;)

It's simply the `PAsData` building cousin of `pconstant`!

## Implementing `PConstant` & `PLift`
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

# PlutusType, PCon, and PMatch
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
This is a [scott encoded representation of the familiar `Maybe` data type](#data-encoding-and-scott-encoding). As you can see, `PInner` of `PMaybe` is actually a Plutarch level function. And that's exactly why `pcon'` creates a *function*. `pmatch'`, then, simply "matches" on the function - scott encoding fashion.

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
If you want to represent your data type with [scott encoding](#data-encoding-and-scott-encoding) (and therefore not let it be `Data` encoded), you should simply derive it generically-
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

If you want to represent your data type as some simple builtin type (e.g integer, bytestrings, string/text, list, or assoc map), you can define your datatype as a `newtype` to the underlying builtin term and derive `PlutusType` using [`DerivePNewtype`](#deriving-typeclasses-for-newtypes).
```hs
import Plutarch.Prelude

newtype MyInt (s :: S) = MyInt (Term s PInteger)
  deriving (PlutusType) via (DerivePNewtype MyInt PInteger)
```

If you don't want it to be a newtype, but rather - an ADT, and still have it be represented as some simple builtin type - you can do so by implementing `PlutusType` manually. Here's an example of encoding a Sum type as an Enum via `PInteger`-
```hs
import Plutarch
import Plutarch.Prelude

data AB (s :: S) = A | B

instance PlutusType AB where
  type PInner AB _ = PInteger

  pcon' A = 0
  pcon' B = 1

  pmatch' x f =
    pif (x #== 0) (f A) (f B)
```
## Implementing `PlutusType` for your own types (Data Encoding)
If your type is supposed to be represented using [`Data` encoding](#data-encoding-and-scott-encoding) instead (i.e has a [`PIsDataRepr`](#pisdatarepr--pdatafields) instance), you can derive `PlutusType` via `PIsDataReprInstances`
```hs
import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch.Prelude
import Plutarch.DataRepr

data MyType (a :: PType) (b :: PType) (s :: S)
  = One (Term s (PDataRecord '[ "_0" ':= a ]))
  | Two (Term s (PDataRecord '[ "_0" ':= b ]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances (MyType a b)
```

# PListLike
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

# PIsDataRepr & PDataFields
`PIsDataRepr` allows for easily constructing *and* deconstructing `Constr` [`BuiltinData`/`Data`](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md) values. It allows fully type safe matching on `Data` values, without embedding type information within the generated script - unlike PlutusTx. `PDataFields`, on top of that, allows for ergonomic field access.

> Aside: What's a `Constr` data value? Briefly, it's how Plutus Core encodes non-trivial ADTs into `Data`/`BuiltinData`. It's essentially a sum-of-products encoding. But you don't have to care too much about any of this. Essentially, whenever you have a custom non-trivial ADT (that isn't just an integer, bytestring, string/text, list, or assoc map), you should implement `PIsDataRepr` for it.

For example, `PScriptContext` - which is the Plutarch synonym to [`ScriptContext`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Contexts.html#t:ScriptContext) - has the necessary instances. This lets you easily keep track of its type, match on it, deconstruct it - you name it!
```hs
-- NOTE: REQUIRES GHC 9!
{-# LANGUAGE QualifiedDo #-}

import Plutarch.Prelude
import Plutarch.Api.Contexts
import qualified Plutarch.Monadic as P

foo :: Term s (PScriptContext :--> PString)
foo = plam $ \ctx -> P.do
  purpose <- pmatch . pfromData $ pfield @"purpose" # ctx
  case purpose of
    PMinting _ -> "It's minting!"
    PSpending _ -> "It's spending!"
    PRewarding _ -> "It's rewarding!"
    PCertifying _ -> "It's certifying!"
```
> Note: The above snippet uses GHC 9 features (`QualifiedDo`). Be sure to check out [how to translate the do syntax to GHC 8](#translating-do-syntax-with-qualifieddo-to-ghc-8).

Of course, just like `ScriptContext` - `PScriptContext` is represented as a `Data` value in Plutus Core. Plutarch just lets you keep track of the *exact representation* of it within the type system.

Here's how `PScriptContext` is defined-
```hs
newtype PScriptContext (s :: S)
  = PScriptContext
      ( Term
          s
          ( PDataRecord
              '[ "txInfo" ':= PTxInfo
               , "purpose" ':= PScriptPurpose
               ]
          )
      )
```
It's a constructor containing a [`PDataRecord`](#pdatasum--pdatarecord) term. It has 2 fields- `txInfo` and `purpose`.

First, we extract the `purpose` field using `pfield @"purpose"`-
```hs
pfield :: Term s (PScriptContext :--> PAsData PScriptPurpose)
```
> Note: When extracting several fields from the same variable, you should instead use `pletFields`. See: [Extracting fields](#all-about-extracting-fields)

Now, we can grab the `PScriptPurpose` from within the `PAsData` using `pfromData`-
```hs
pfromData :: Term s (PAsData PScriptPurpose) -> Term s PScriptPurpose
```
Finally, we can `pmatch` on it to extract the Haskell ADT (`PScriptPurpose s`) out of the Plutarch term-
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

## All about extracting fields
We caught a glimpse of field extraction in the example above, thanks to `pfield`. However, that barely touched the surface.

Once a type has a `PDataFields` instance, field extraction can be done with these 3 functions-
* `pletFields`
* `pfield`
* `hrecField` (when not using `OverloadedRecordDot` or record dot preprocessor)

Each has its own purpose. However, `pletFields` is arguably the most general purpose and most efficient. Whenever you need to extract several fields from the same variable, you should use `pletFields`-
```hs
-- NOTE: REQUIRES GHC 9!
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Plutarch.Prelude
import Plutarch.Api.Contexts
import qualified Plutarch.Monadic as P

foo :: Term s (PScriptContext :--> PUnit)
foo = plam $ \ctx' -> P.do
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  let
    purpose = ctx.purpose
    txInfo = ctx.txInfo
  -- <use purpose and txInfo here>
  pconstant ()
```
> Note: The above snippet uses GHC 9 features (`QualifiedDo` and `OverloadedRecordDot`). Be sure to check out [how to translate the do syntax to GHC 8](#translating-do-syntax-with-qualifieddo-to-ghc-8) and [alternatives to `OverloadedRecordDot`](#alternatives-to-overloadedrecorddot).

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
> Aside: Of course, we used the convenient `do` syntax provided to us by `Plutarch.Monadic` to write the continuation merely as a `<-` bind. Without do notation, you'd have to write-
>
> ```hs
> pletFields @["txInfo", "purpose"] ctx' $ \ctx ->
>   let
>     purpose = ctx.purpose
>     txInfo = ctx.txInfo
>   in pconstant ()
> ```

You can then access the fields on this `HRec` using `OverloadedRecordDot`.

Next up is `pfield`. You should *only ever* use this if you just want one field from a variable and no more. It's usage is simply `pfield @"fieldName" # variable`. You can, however, also use `pletFields` in this case (e.g `pletFoelds @'["fieldName"] variable`). `pletFields` with a singular field has the same efficiency as `pfield`!

Finally, `hrecField` is merely there to supplement the lack of record dot syntax. See: [Alternative to `OverloadedRecordDot`](#alternative-to-overloadedrecorddot).

### Alternatives to `OverloadedRecordDot`
If `OverloadedRecordDot` is not available, you can also try using the [record dot preprocessor plugin](https://hackage.haskell.org/package/record-dot-preprocessor).

If you don't want to use either, you can simply use `hrecField`. In fact, `ctx.purpose` above just translates to `hrecField @"purpose" ctx`. Nothing magical there!

## All about constructing data values
We learned about type safe matching (through `PlutusType`) as well as type safe field access (through `PDataFields`) - how about construction? Since `PIsDataRepr` allows you to derive [`PlutusType`](#plutustype-pcon-and-pmatch), and `PlutusType` bestows the ability to not only *deconstruct*, but also **construct** values - you can do that just as easily!

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

What's more interesting, is the `fields` binding. Recall that `PMinting` is a constructor with one argument, that argument is a [`PDataRecord`](#pdatasum--pdatarecord) term. In particular, we want: `Term s (PDataRecord '["_0" ':= PCurrencySymbol ])`. It encodes the exact type, position, and name of the field. So, all we have to do is create a `PDataRecord` term!

Of course, we do that using `pdcons` - which is just the familiar `cons` specialized for `PDataRecord` terms.
```hs
pdcons :: forall label a l s. Term s (PAsData a :--> PDataRecord l :--> PDataRecord ((label ':= a) ': l))
```
It takes a `PAsData a` and adds that `a` to the `PDataRecord` heterogenous list. We feed it a `PAsData PCurrencySymbol` and `pdnil` - the empty data record. That should give us-
```hs
pdcons # currSymDat # pdnil :: Term _ (PDataRecord '[ label ':= PCurrencySymbol ])
```
Cool! Wait, what's `label`? It's the field name associated with the field, in our case, we want the field name to be `_0` - because that's what the `PMinting` constructor wants. You can either specify the label with a type application or you can just have a type annotation for the binding (which is what we do here). Or you can let GHC try and match up the `label` with the surrounding environment!

Now that we have `fields`, we can use it with `PMinting` to build a `PScriptPurpose s` and feed it to `pcon` - we're done!

## Implementing PIsDataRepr and friends
Implementing these is rather simple with generic deriving + `PIsDataReprInstances`. All you need is a well formed type using `PDataRecord`. For example, suppose you wanted to implement `PIsDataRepr` for the Plutarch version of this Haskell type-
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
> Also see: [Isomorphism between Haskell ADTs and `PIsDataRepr`](#the-isomorphism-between-makeisdataindexed-haskell-adts-and-pisdatarepr)

And you'd simply derive `PIsDataRepr` using generics. However, you **must** also derive `PIsData` using `PIsDataReprInstances`. Moreover, you should also derive `PlutusType`. For single constructor data types, you should also derive `PDataFields`.

> Aside: If your type is *not* a sumtype, but rather a newtype with a single constructor - you should also derive `PDataFields`. In the case of sumtypes, the existing `PDataFields` instance for `PDataRecord` will be enough.

Combine all that, and you have-
```hs
import qualified GHC.Generics as GHC
import Generics.SOP (Generic)

import Plutarch.Prelude
import Plutarch.DataRepr

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
> Note: You cannot implement `PIsDataRepr` for types that are represented using [scott encoding](#data-encoding-and-scott-encoding). Your types must be well formed and should be using `PDataRecord` terms instead.

That's it! Now you can represent `PVehicle` as a `Data` value, as well as deconstruct and access its fields super ergonomically. Let's try it!
```hs
-- NOTE: REQUIRES GHC 9!
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OverloadedRecordDot #-}

import qualified Plutarch.Monadic as P
import Plutarch.Prelude

test :: Term s (PVehicle :--> PInteger)
test = plam $ \veh' -> P.do
  veh <- pmatch veh'
  case veh of
    PFourWheeler fwh' -> P.do
      fwh <- pletFields @'["_0", "_1", "_2", "_3"] fwh'
      pfromData fwh._0 + pfromData fwh._1 + pfromData fwh._2 + pfromData fwh._3
    PTwoWheeler twh' -> P.do
      twh <- pletFields @'["_0", "_1"] twh'
      pfromData twh._0 + pfromData twh._1
    PImmovableBox _ -> 0
```
> Note: The above snippet uses GHC 9 features (`QualifiedDo` and `OverloadedRecordDot`). Be sure to check out [how to translate the do syntax to GHC 8](#translating-do-syntax-with-qualifieddo-to-ghc-8) and [alternatives to `OverloadedRecordDot`](#alternatives-to-overloadedrecorddot).

What about types with singular constructors? It's quite similar to the sum type case. Here's how it looks-
```hs
{-# LANGUAGE UndecidableInstances #-}

import qualified GHC.Generics as GHC
import Generics.SOP (Generic)

import Plutarch.Prelude
import Plutarch.DataRepr

newtype PFoo (s :: S) = PMkFoo (Term s (PDataRecord '["foo" ':= PByteString]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PFoo
```
Just an extra `PDataFields` derivation compared to the sum type usage! (oh and also the ominous `UndecidableInstances`)
