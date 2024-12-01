<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.DerivingGenerics (PMyType (..)) where
import Plutarch.Prelude
import Data.Kind (Type)
import GHC.Generics (Generic)
```

</p>
</details>

# Deriving typeclasses with generics

Plutarch also provides sophisticated generic deriving support for completely custom types. In particular, you can easily derive `PlutusType` for your own type:

```haskell
data PMyType (a :: S -> Type) (b :: S -> Type) (s :: S)
  = POne (Term s a)
  | PTwo (Term s b)
  deriving stock Generic
  deriving anyclass PlutusType
instance DerivePlutusType (PMyType a b) where type DPTStrat _ = PlutusTypeScott
```

> Note: This requires the `generics-sop` package.

This will use a [Scott encoding representation](./../Concepts/DataAndScottEncoding.md#scott-encoding) for `MyType`, which is typically what you want.
If you want to use [data encoding representation](./../Concepts/DataAndScottEncoding.md) instead in your `PlutusType` instance - you should derive it
using `PlutusTypeData`. Check out: [implementing `PIsDataRepr` and friends](./../Typeclasses/PIsDataReprAndPDataFields.md#implementing-pisdatarepr-and-friends)

Currently, generic deriving supports the following typeclasses:

- [`PlutusType`](./../Typeclasses/PlutusType,PCon,PMatch.md#implementing-plutustype-for-your-own-types-scott-encoding) (Scott encoding only)
- [`PEq`](./../Typeclasses/PEqAndPOrd.md)
- [`POrd`](./../Typeclasses/PEqAndPOrd.md)
- [`PTryFrom`](./../Typeclasses/PTryFrom.md)
- `PShow`
- `PIsData`
- `PDataFields`
