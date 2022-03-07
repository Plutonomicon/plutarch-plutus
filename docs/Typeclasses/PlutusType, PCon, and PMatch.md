# `PlutusType`, `PCon`, and `PMatch`

`PlutusType` is the primary typeclass that determines the underlying representation for a Plutarch type. It lets you construct and deconstruct Plutus Core constants from a Plutarch type's constructors (possibly containing other Plutarch terms). It's essentially a combination of `PCon` (for term construction) and `PMatch` (for term deconstruction).

```hs
class (PCon a, PMatch a) => PlutusType (a :: k -> Type) where
  type PInner a (b' :: k -> Type) :: k -> Type
  pcon' :: forall s b. a s -> Term s (PInner a b)
  pmatch' :: forall s b. Term s (PInner a b) -> (a s -> Term s b) -> Term s b
```

> Note: You don't need to look too much into the types! After all, you'll be using `pcon` and `pmatch`, rather than `pcon'` and `pmatch'`.
> `PInner` is meant to represent the "inner" type of `a` - the Plutarch type representing the Plutus Core constant used to represent `a`.

Here's the `PlutusType` instance for `PMaybe`:

```hs
data PMaybe a s = PJust (Term s a) | PNothing

instance PlutusType (PMaybe a) where
  type PInner (PMaybe a) b = (a :--> b) :--> PDelayed b :--> b
  pcon' (PJust x) = plam $ \f (_ :: Term _ _) -> f # x
  pcon' PNothing = plam $ \_ g -> pforce g
  pmatch' x f = x # (plam $ \inner -> f (PJust inner)) # (pdelay $ f PNothing)
```

This is a [Scott encoded representation of the familiar `Maybe` data type](./../Concepts/Data%20and%20Scott%20encoding.md#scott-encoding). As you can see, `PInner` of `PMaybe` is actually a Plutarch level function. And that's exactly why `pcon'` creates a _function_. `pmatch'`, then, simply "matches" on the function - Scott encoding fashion.

You should always use `pcon` and `pmatch` instead of `pcon'` and `pmatch'` - these are provided by the `PCon` and `PMatch` typeclasses:

```hs
class PCon a where
  pcon :: a s -> Term s a

class PMatch a where
  pmatch :: Term s a -> (a s -> Term s b) -> Term s b
```

All `PlutusType` instances get `PCon` and `PMatch` instances for free!

For types that cannot easily be both `PCon` and `PMatch` - feel free to implement just one of them! However, in general, **prefer implementing `PlutusType`**!

## Implementing `PlutusType` for your own types (Scott Encoding)

If you want to represent your data type with [Scott encoding](./../Concepts/Data%20and%20Scott%20encoding.md#scott-encoding) (and therefore don't need to make it `Data` encoded), you should simply derive it generically:

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

If your type is supposed to be represented using [`Data` encoding](./../Concepts/Data%20and%20Scott%20encoding.md#data-encoding) instead (i.e. has a [`PIsDataRepr`](./PIsDataRepr%20and%20PDataFields.md) instance), you can derive `PlutusType` via `PIsDataReprInstances`:

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

See: [Implementing `PIsDataRepr` and friends](./PIsDataRepr%20and%20PDataFields.md#implementing-pisdatarepr-and-friends).

## Implementing `PlutusType` for your own types (`newtype`)

See: [`DerivePNewtype`](./../Usage/Deriving%20for%20newtypes.md).
