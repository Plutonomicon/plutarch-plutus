This section describes the primary typeclasses used in Plutarch.

> Note: If you spot any mistakes/have any related questions that this guide lacks the answer to, please don't
  hesitate to raise an issue. The goal is to have high quality documentation for Plutarch users!

- [`PEq` & `POrd`](./Typeclasses/PEqAndPOrd.md)
- [`PIntegral`](./Typeclasses/PIntegral.md)
- [`PIsData`](./Typeclasses/PIsData.md)
- [`PlutusType`, `PCon`, and `PMatch`](./Typeclasses/PlutusType,PCon,PMatch.md)
  - [Implementing `PlutusType` for your own types (Scott Encoding)](./Typeclasses/PlutusType,PCon,PMatch.md#implementing-plutustype-for-your-own-types-scott-encoding)
  - [Implementing `PlutusType` for your own types (`Data` Encoding)](./Typeclasses/PlutusType,PCon,PMatch.md#implementing-plutustype-for-your-own-types-data-encoding)
  - [Implementing `PlutusType` for your own types (`newtype`)](./Typeclasses/PlutusType,PCon,PMatch.md#implementing-plutustype-for-your-own-types-newtype)
- [`PConstant` & `PLift`](./Typeclasses/PConstant and PLift.md)
  - [Implementing `PConstant` & `PLift`](./Typeclasses/PConstantAndPLift.md#implementing-pconstant--plift)
  - [Implementing `PConstant` & `PLift` for types with type variables (generic types)](./Typeclasses/PConstantAndPLift.md#implementing-pconstant--plift-for-types-with-type-variables-generic-types)
- [`PListLike`](./Typeclasses/PListLike.md)
- [`PIsDataRepr` & `PDataFields`](./Typeclasses/PIsDataReprAndPDataFields.md)
  - [All about extracting fields](./Typeclasses/PIsDataReprAndPDataFields.md#all-about-extracting-fields)
    - [Alternatives to `OverloadedRecordDot`](./Typeclasses/PIsDataReprAndPDataFields.md#alternatives-to-overloadedrecorddot)
  - [All about constructing data values](./Typeclasses/PIsDataReprAndPDataFields.md#all-about-constructing-data-values)
  - [Implementing `PIsDataRepr` and friends](./Typeclasses/PIsDataReprAndPDataFields.md#implementing-pisdatarepr-and-friends)
- [`PTryFrom`](./Typeclasses/PTryFrom.md)
  - [Laws](./Typeclasses/PTryFrom.md#laws)
  - [`PTryFromExcess`](./Typeclasses/PTryFrom.md#ptryfromexcess)
  - [Recovering only partially](./Typeclasses/PTryFrom.md#recovering-only-partially)
