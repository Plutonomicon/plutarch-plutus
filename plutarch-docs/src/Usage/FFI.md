<details>
<summary> imports </summary>
<p>

```hs
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE TemplateHaskell #-}
module Plutarch.Docs.FFI () where 
import Plutarch.Prelude
import Plutarch.FFI

import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import PlutusTx.Builtins.Internal qualified as PlutusTx

import Generics.SOP qualified as SOP
import Data.Default (def)
```

</p>
</details>

# Interoperability with PlutusTx

> Note: This module needs the PlutusTx plugin. As it does not yet work under `ghc92`+, some of this documentation is not part 
> of the CI, the documentation is up to date as of Plutarch 1.3.

If you already have a codebase built using PlutusTx, you can choose to
re-write only its critical parts in Plutarch and to call them from
PlutusTx. The function to use is `Plutarch.FFI.foreignExport`:

```hs
doubleInPlutarch :: ClosedTerm (PInteger :--> PInteger)
doubleInPlutarch = plam (2 *)

doubleExported :: PlutusTx.CompiledCode (Integer -> Integer)
doubleExported = foreignExport def doubleInPlutarch

doubleUseInPlutusTx :: PlutusTx.CompiledCode Integer
doubleUseInPlutusTx = doubleExported `PlutusTx.applyCode` PlutusTx.liftCode 21
```

Alternatively, you may go in the opposite direction and call an existing
PlutusTx function from Plutarch using `Plutarch.FFI.foreignImport`:

```hs
doubleInPlutusTx :: PlutusTx.CompiledCode (Integer -> Integer)
doubleInPlutusTx = $$(PlutusTx.compile [||(2 *) :: Integer -> Integer||])

doubleImported :: Term s (PInteger :--> PInteger)
doubleImported = foreignImport doubleInPlutusTx

doubleUseInPlutarch :: Term s PInteger
doubleUseInPlutarch = doubleImported # 21
```

Note how Plutarch type `PInteger :--> PInteger` corresponds to Haskell
function type `Integer -> Integer`. If the types didn't correspond, the
`foreignExport` and `foreignImport` applications wouldn't compile. The
following table shows the correspondence between the two universes of types:

| Plutarch       | Haskell             |
| -------------- | ------------------- |
| `pa :--> pb`   | `a -> b`            |
| `PTxList pa`   | `[a]`               |
| `PTxMaybe pa`  | `Maybe a`           |
| `PInteger`     | `Integer`           |
| `PBool`        | `BuiltinBool`       |
| `PString`      | `BuiltinString`     |
| `PByteString`  | `BuiltinByteString` |
| `PBuiltinData` | `Data`              |
| `PUnit`        | `BuiltinUnit`       |
| `PDelayed pa`  | `Delayed a`         |

## User-defined types

When it comes to user-defined types, you have a choice of passing their values
encoded as `Data` or directly. In the latter case, you'll have to declare your
type twice with two kinds: as a Haskell `Type` and as a Plutarch
`PType`. Furthermore, both types must be instances of `SOP.Generic`, as in this
example:

```hs
data SampleRecord = SampleRecord
  { sampleBool :: PlutusTx.BuiltinBool
  , sampleInt :: PlutusTx.Integer
  , sampleString :: PlutusTx.BuiltinString
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)

data PSampleRecord (s :: S) = PSampleRecord
  { psampleBool :: Term s PBool
  , psampleInt :: Term s PInteger
  , psampleString :: Term s PString
  }
  deriving stock Generic
  deriving anyclass (SOP.Generic, PlutusType)
instance DerivePlutusType PSampleRecord where type DPTStrat _ = PlutusTypeScott
```

With these two declarations in place, the preceding table can gain another
row:

| Plutarch                  | Haskell              |
| -----------------------   | -------------------- |
| `PDelayed PSampleRecord`  | `SampleRecord`       |

The reason for `PDelayed` above is a slight difference in Scott encodings of
data types between Plutarch and PlutusTx. It means you'll need to apply
`pdelay` to a `PSampleRecord` value before passing it through FFI to Haskell,
and `pforce` after passing it in the opposite direction.

This technique can be used for most data types, but it doesn't cover recursive
types (such as lists) nor data types with nullary constructors (such as
`Maybe`). To interface with these two common Haskell types, use `PTxMaybe` and
`PTxList` types from `Plutarch.FFI`. The module also exports the means to
convert between these special purpose types and the regular Plutarch `PMaybe`
and `PList`.
