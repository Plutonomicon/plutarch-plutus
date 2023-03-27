<details>
<summary> imports </summary>
<p>

```haskell
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE UndecidableInstances #-}
module Plutarch.Docs.PConstantAndPLift (b, purp) where 
import Plutarch.Prelude
import PlutusLedgerApi.V1.Contexts (ScriptPurpose (Minting))
import Plutarch.Api.V1.Contexts (PScriptPurpose)
```

</p>
</details>

# `PConstant` & `PLift`

These two closely tied together typeclasses establish a bridge between a Plutarch level type (that is represented 
as a builtin type, i.e. 
[`DefaultUni`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-core/html/PlutusCore.html#t:DefaultUni)) 
and its corresponding [Haskell synonym](./../Concepts/Haskell%20Synonym.md). The gory details of these 
two are not too useful to users, but you can read all about it if you want at 
[Developers' corner](../DEVGUIDE.md#pconstant-and-plift).

What's more important, are the abilities that `PConstant`/`PLift` instances have:

```hs
pconstant :: PLift p => PLifted p -> Term s p

plift :: (HasCallStack, PLift p) => ClosedTerm p -> PLifted p
```

These typeclasses also bestow the associated type families:

```hs
type PLifted :: PType -> Type

type PConstanted :: Type -> PType
```

These are meant to be inverse type families of each other. In particular, `PLifted p` represents the Haskell synonym 
of the Plutarch type, `p`. Similarly, `PConstanted h` represents the Plutarch type corresponding to the Haskell type, 
`h`.

`pconstant` lets you build a Plutarch value from its corresponding Haskell synonym. For example, the Haskell synonym 
of [`PBool`](./../Types/PBool.md) is 
[`Bool`](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Bool.html#t:Bool).

```haskell
b :: Term s PBool
b = pconstant False
```

On the other end, `plift` lets you obtain the Haskell synonym of a Plutarch value (that is represented as a builtin value, i.e. [`DefaultUni`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-core/html/PlutusCore.html#t:DefaultUni)):

```haskell
purp :: Term s PScriptPurpose
purp = pconstant $ Minting "be"
```

```hs
> plift purp
Minting "be"
```

There's also another handy utility, `pconstantData`:

```hs
pconstantData :: (PLift p, ToData (PLifted p)) => PLifted p -> Term s (PAsData p)
```

> Note: This isn't the actual type of `pconstantData` - it's simplified here for the sake of documentation ;)

It's simply the `PAsData` building cousin of `pconstant`!

## Implementing `PConstant` & `PLift`

If your custom Plutarch type is represented by a builtin type under the hood 
(i.e. not [Scott encoded](./../Concepts/Data%20and%20Scott%20encoding.md#scott-encoding) - rather one of the 
[`DefaultUni`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-core/html/PlutusCore.html#t:DefaultUni) types) 
- you can implement `PLift` for it by using the provided machinery.

This comes in three flavors:

- Plutarch type represented **directly** by a builtin type that **is not** `Data` (`DefaultUniData`) 
  ==> `DerivePConstantDirect`

  *Ex:* `PInteger` is directly represented as a builtin integer.

- Plutarch type represented **indirectly** by a builtin type that **is not** `Data` (`DefaultUniData`) 
  ==> `DerivePConstantViaNewtype`

  *Ex:* `PPubKeyHash` is a newtype to a `PByteString`, and `PByteString` is _directly_ represented as a 
  `BuiltingBytestring`.

- Plutarch type represented by `Data`, i.e. [data encoded](./../Concepts/Data%20and%20Scott%20encoding.md#data-encoding)
  (`DefaultUniData`) ==> `DerivePConstantViaData`

  *Ex:* `PScriptPurpose` is represented as a `Data` value. It is synonymous to `ScriptPurpose` from the Plutus Ledger API

Whichever path you need to go down, there is one common part- implementing `PLift`, or rather `PUnsafeLiftDecl`. See, 
`PLift` is actually just a type synonym to `PUnsafeLiftDecl` (with a bit more machinery). Essentially an empty 
typeclass with an associated type family that provides insight on the relationship between a Plutarch type and its 
Haskell synonym.

```hs
instance PUnsafeLiftDecl YourPlutarchType where
  type PLifted YourPlutarchType = YourHaskellType
```

In fact, `PConstant` is _also_ a type synonym. The actual typeclass you'll be implementing is `PConstantDecl`.

You're tasked with assigning the correct Haskell synonym to your Plutarch type, and what an important task it is! Recall that `pconstant`'s argument type will depend on your assignment here. In particular: `pconstant :: YourHaskellType -> YourPlutarchType`.

Some examples:

- for `YourPlutarchType` = `PInteger`, `YourHaskellType` = `Integer`

  ```hs
  instance PUnsafeLiftDecl PInteger where type PLifted PInteger = Integer
  ```
- for `YourPlutarchType` = `PValidatorHash`, `YourHaskellType` = `ValidatorHash`

  ```hs
  instance PUnsafeLiftDecl PValidatorHash where type PLifted PValidatorHash = Plutus.ValidatorHash
  ```
- for `YourPlutarchType` = `PScriptPurpose`, `YourHaskellType` = `ScriptPurpose`

  ```hs
  instance PUnsafeLiftDecl PScriptPurpose where type PLifted PScriptPurpose = Plutus.ScriptPurpose
  ```

Now, let's get to implementing `PConstant` for the Haskell synonym, via the three methods. 
The first of which is `DerivePConstantDirect`:

```hs
deriving via (DerivePConstantDirect Integer PInteger) instance PConstantDecl Integer
```

`DerivePConstantDirect` takes in two type parameters:

- The Haskell type itself, for which `PConstant` is being implemented for.
- The **direct** Plutarch synonym to the Haskell type.

Pretty simple! Let's check out `DerivePConstantViaNewtype` now:

```hs
{-# LANGUAGE UndecidableInstances #-}

import Plutarch.Lift (DerivePConstantViaNewtype (DerivePConstantViaNewtype), PConstantDecl, PUnsafeLiftDecl)
import Plutarch.Prelude

import qualified Plutus.V1.Ledger.Api as Plutus

newtype PValidatorHash (s :: S) = PValidatorHash (Term s PByteString)

...

deriving via (DerivePConstantViaNewtype Plutus.ValidatorHash PValidatorHash PByteString)
  instance PConstantDecl Plutus.ValidatorHash
```

`DerivePConstantViaNewtype` takes in three type parameters:

- The Haskell newtype itself, for which `PConstant` is being implemented for.
- The Plutarch synonym to the Haskell type.
- The actual Plutarch type corresponding to the Haskell type contained within the newtype.

  For example, `ValidatorHash` is a newtype to a `ByteString`, which is synonymous to `PByteString`. In the same way, `PValidatorHash` is actually just a newtype to a `PByteString` term.
  During runtime, `ValidatorHash` is actually just a `ByteString`, the same applies for `PValidatorHash`. So we give it the `newtype` treatment with `DerivePConstantViaNewtype`!

Finally, we have `DerivePConstantViaData` for `Data` values:

```hs
data PScriptPurpose (s :: S)
  = PMinting (Term s (PDataRecord '["_0" ':= PCurrencySymbol]))
  | PSpending (Term s (PDataRecord '["_0" ':= PTxOutRef]))
  | PRewarding (Term s (PDataRecord '["_0" ':= PStakingCredential]))
  | PCertifying (Term s (PDataRecord '["_0" ':= PDCert]))

deriving via 
  (DerivePConstantViaData Plutus.ScriptPurpose PScriptPurpose) 
  instance PConstantDecl Plutus.ScriptPurpose
```

`DerivePConstantViaData` takes in two type parameters:

- The Haskell type itself, for which `PConstant` is being implemented for.
- The Plutarch synonym to the Haskell type.
  And that's all you need to know to implement `PConstant` and `PLift`!

## Implementing `PConstant` & `PLift` for types with type variables (generic types)

If your Plutarch type and its Haskell synonym are generic types (e.g. `PMaybeData a`) - the implementation gets a tad more difficult. In particular, you need to constrain the generic type variables to also have the relevant instance.

The constraints observed when implementing `PLift`:

- Each type variable must also have a `PLift` instance.

The constraints observed when implementing `PConstant`:

- Each type variable must also have a `PConstant` instance.

If you're using `DerivePConstantViaData`, you should use the `PLiftData` and `PConstantData` constraints instead respectively.

Here's how you'd set up all this for `PMaybeData a`:

```hs
data PMaybeData a (s :: S)
  = PDJust (Term s (PDataRecord '["_0" ':= a]))
  | PDNothing (Term s (PDataRecord '[]))

instance PLiftData a => PUnsafeLiftDecl (PMaybeData a) where
  type PLifted (PMaybeData a) = Maybe (PLifted a)

deriving via
  (DerivePConstantViaData (Maybe a) (PMaybeData (PConstanted a)))
  instance
    PConstantData a => PConstantDecl (Maybe a)
```
