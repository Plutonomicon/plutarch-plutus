<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.DerivingGenerics (PMyType (..)) where 
import Plutarch.Prelude
```

</p>
</details>

# Deriving typeclasses with generics

Plutarch also provides sophisticated generic deriving support for completely custom types. In particular, you can easily derive `PlutusType` for your own type:

```haskell
data PMyType (a :: PType) (b :: PType) (s :: S)
  = POne (Term s a)
  | PTwo (Term s b)
  deriving stock Generic
  deriving anyclass PlutusType
instance DerivePlutusType (PMyType a b) where type DPTStrat _ = PlutusTypeScott
```

> Note: This requires the `generics-sop` package.

This will use a [Scott encoding representation](./../Concepts/Data%20and%20Scott%20encoding.md#scott-encoding) for `MyType`, which is typically what you want. 
If you want to use [data encoding representation](./../Concepts/Data%20and%20Scott%20encoding.md) instead in your `PlutusType` instance - you should derive it 
using `PlutusTypeData`. Check out: [implementing `PIsDataRepr` and friends](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#implementing-pisdatarepr-and-friends)

Currently, generic deriving supports the following typeclasses:

- [`PlutusType`](./../Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md#implementing-plutustype-for-your-own-types-scott-encoding) (Scott encoding only)
- [`PEq`](./../Typeclasses/PEq%20and%20POrd.md)
- [`POrd`/ `PPartialOrd`](./../Typeclasses/PEq%20and%20POrd.md)
- [`PTryFrom`](./../Typeclasses/PTryFrom.md)
- `PShow`
- `PIsData`
- `PDataFields`
