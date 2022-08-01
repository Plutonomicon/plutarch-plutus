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

This will use a [Scott encoding representation](./../Concepts/Data%20and%20Scott%20encoding.md#scott-encoding) for `MyType`, which is typically what you want. If you want to use [data encoding representation](./../Concepts/Data%20and%20Scott%20encoding.md) instead in your `PlutusType` instance - you should derive it using `PIsDataReprInstances`. Check out: [implementing `PIsDataRepr` and friends](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#implementing-pisdatarepr-and-friends)

Currently, generic deriving supports the following typeclasses:

- [`PlutusType`](./../Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md#implementing-plutustype-for-your-own-types-scott-encoding) (Scott encoding only)
- [`PIsDataRepr`](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#implementing-pisdatarepr-and-friends)
- [`PEq`](./../Typeclasses/PEq%20and%20POrd.md)
