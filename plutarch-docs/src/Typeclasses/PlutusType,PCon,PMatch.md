<details>
<summary> imports </summary>
<p>

```haskell
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE UndecidableInstances #-}
module Plutarch.Docs.PlutusTypePConAndPMatch (PMyType(..), PMyTypeData(..)) where
import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude
import Plutarch.Repr.SOP (DeriveAsSOPStruct (DeriveAsSOPStruct))
```

</p>
</details>

# `PlutusType`

`PlutusType` is the primary typeclass that determines the underlying representation for a Plutarch type. It lets you construct and deconstruct Plutus Core constants from a Plutarch type's constructors
(possibly containing other Plutarch terms).

> NOTE: It's essentially a combination of `PCon` (for term construction) and `PMatch` (for term deconstruction). Nowadays, `PCon` and `PMatch` are actually both
> just an alias for `PlutusType` and you'll get a deprecation warning if you use them.

```hs
class PlutusType (a :: S -> Type) where
  {-
    snip
  -}
  pcon' :: forall s. a s -> Term s (PInner a)
  default pcon' :: DerivePlutusType a => forall s. a s -> Term s (PInner a)
  pcon' = let _ = witness (Proxy @(PlutusType a)) in derivedPCon

  pmatch' :: forall s b. Term s (PInner a) -> (a s -> Term s b) -> Term s b
  default pmatch' :: DerivePlutusType a => forall s b. Term s (PInner a) -> (a s -> Term s b) -> Term s b
  pmatch' = derivedPMatch

```
> Note: You don't need to look too much into the types! After all, you'll be using `pcon` and `pmatch`, rather than `pcon'` and `pmatch'`.
> `PInner` is meant to represent the "inner" type of `a` - the Plutarch type representing the Plutus Core constant used to represent `a`.

You should always use `pcon` and `pmatch` instead of `pcon'` and `pmatch'` - these are provided by the `PCon` and `PMatch` typeclasses:

Another feature of `PlutusType` instances is that you can extract out the *inner* type of any `PlutusType` instance! Above, the inner type
(or representation) of `PMaybe` was a function. You can use `pto` to safely take this inner type out-

```hs
pto :: Term s a -> Term s (PInner a)
```

This is quite useful when working with `newtype`s. Notice how `PCurrencySymbol`, for example, is simply a newtype to a `PByteString`. Its
`PInner` is also `PByteString`. To be able to use functions that operate on `PByteString`s with your `PCurrencySymbol`, you can simply take
out the `PByteString` using `pto`!

## Implementing `PlutusType` for your own types (Scott Encoding)

If you want to represent your data type with [Scott encoding](./../Concepts/DataAndScottEncoding.md#scott-encoding) (and therefore
don't need to make it `Data` encoded), you should simply derive it generically:

```haskell
data PMyType (a :: S -> Type) (b :: S -> Type) (s :: S)
  = POne (Term s a)
  | PTwo (Term s b)
  deriving stock (Generic)
  deriving anyclass PlutusType

instance DerivePlutusType (PMyType a b) where type DPTStrat _ = PlutusTypeScott
```

> NOTE: you can derive PlutusType for all types you defined a `DerivePlutusType` instance for. The strategy it uses is determined by the
> type that you put after `DPTStrat _ =`, in this case Scottencoding

## Implementing `PlutusType` for your own types (`Data` Encoding)

If your type is supposed to be represented using [`Data` encoding](./../Concepts/DataAndScottEncoding.md#data-encoding) instead,
you can derive `PlutusType` via `PlutusTypeData`:

```haskell
data PMyTypeData (a :: S -> Type) (b :: S -> Type) (s :: S)
  = POneD (Term s (PDataRecord '[ "_0" ':= a ]))
  | PTwoD (Term s (PDataRecord '[ "_0" ':= b ]))
  deriving stock Generic
  deriving anyclass (PlutusType)

instance DerivePlutusType (PMyTypeData a b) where type DPTStrat _ = PlutusTypeData
```

## Implementing `PlutusType` for your own types (SoP Encoding)

Mechanism is the same as in Scott encoding case but using `DeriveAsSOPStruct` instead.

> NOTE: You must import `DeriveAsSOPStruct` with constructor (like imports at the top of this page do) for GHC to see that the helper is coercible to your own type.

```haskell
data PMaybeSoP (a :: S -> Type) (s :: S)
  = PJustSoP (Term s a)
  | PNothingSoP
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PEq, PShow)

deriving via DeriveAsSOPStruct (PMaybeSoP a) instance PlutusType (PMaybeSoP a)
```

## Implementing `PlutusType` for your own types (`newtype`)

See: [`DPTStrat PlutusTypeNewtype`](./../Usage/DerivingForNewtypes.md).
