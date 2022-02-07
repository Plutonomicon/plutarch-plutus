# Revision history for plutarch

# Unreleased

- `TermCont`: Parametrize by result type; add `MonadFail` instance; etc.

  Also, export from `Plutarch.TermCont`, and then from `Plutarch.Prelude` (TermCont is no longer exported by `Plutarch.Internal`).

  Started by: [#226](https://github.com/Plutonomicon/plutarch/pull/226)

- Add `PlutusType` generic deriving support for data encoded Plutarch types, via `PIsDataRepr` and `PIsDataReprInstances`.

  All existing ledger api types now have `PlutusType` instances - not just `PMatch`.

  Started by: [#250](https://github.com/Plutonomicon/plutarch/pull/250)

- Add `PDataRecord` construction utilities, necessary for full usage of data encoded `PlutusType` instances.

  In particular, you can build `PDataRecord`s with `pdcon` and `pdnil` - refer to the guide for more info.

  `pdcon` and `pdnil` are also exported from `Plutarch.Prelude`.

  Module: `Plutarch.DataRepr`

  Started by: [#250](https://github.com/Plutonomicon/plutarch/pull/250)

- Export `PLabeledType ((:=))` from `Plutarch.Prelude`.

  Added by: [#250](https://github.com/Plutonomicon/plutarch/pull/250)

# 1.1.0

- General repository changes.
  - The Plutarch repo has moved to GHC 9.2.1. Projects using GHC 8.10.7 should still be able to depend on Plutarch. There is CI in place to ensure compatibility.

    Relevant PR: [#86](https://github.com/Plutonomicon/plutarch/pull/86)

    CI added by: [#188](https://github.com/Plutonomicon/plutarch/pull/188)
  - Major nix updates.

    Started by: [#75](https://github.com/Plutonomicon/plutarch/pull/75)
  - Benchmarks and perf diffing on PRs to keep track of performance regressions.

    Worked on through:
    * [#102](https://github.com/Plutonomicon/plutarch/pull/102)
    * [#144](https://github.com/Plutonomicon/plutarch/pull/144)
    * [#146](https://github.com/Plutonomicon/plutarch/pull/146)
    * [#164](https://github.com/Plutonomicon/plutarch/pull/164)
    * [#167](https://github.com/Plutonomicon/plutarch/pull/167)
    * [#178](https://github.com/Plutonomicon/plutarch/pull/178)

- Significantly improve `plam` type inference - works seamlessly now (!!!)

  Worked on through:
  * [#29](https://github.com/Plutonomicon/plutarch/pull/29)
  * [#149](https://github.com/Plutonomicon/plutarch/pull/149)
  * [#162](https://github.com/Plutonomicon/plutarch/pull/162)
  * [#168](https://github.com/Plutonomicon/plutarch/pull/168)
  * [#170](https://github.com/Plutonomicon/plutarch/pull/170)

  Relevant issue: [#2](https://github.com/Plutonomicon/plutarch/issues/2)
- Many, many optimizations on generated code.

  Worked on through:
  * [#34](https://github.com/Plutonomicon/plutarch/pull/34)
  * [#37](https://github.com/Plutonomicon/plutarch/pull/37)
  * [#42](https://github.com/Plutonomicon/plutarch/pull/42)
  * [#44](https://github.com/Plutonomicon/plutarch/pull/44)
- Add haddocks and examples to many functions

  Started by: [#49](https://github.com/Plutonomicon/plutarch/pull/49)
- Add many new utility functions, instances, and builtin function synonyms.

  <details>
  <summary> Breakdown of additions </summary>

  - Boolean utilities:- `pnot`, `#&&`/`pand`/`pand'`, `#||`/`por`/`por'`

    Module: `Plutarch.Bool`

    Added in: [#30](https://github.com/Plutonomicon/plutarch/pull/30)
  - ByteString utilities:- `pconsBS`, `psliceBS`, `plengthBS`, `pindexBS`

    Module: `Plutarch.ByteString`

    Added in: [#30](https://github.com/Plutonomicon/plutarch/pull/30)
  - Cryptographic hashing utilities:- `psha2_256`, `psha3_256`, `pblake2b_256`, `pverifySignature`

    Module: `Plutarch.Crypto`

    Added in: [#30](https://github.com/Plutonomicon/plutarch/pull/30)
  - `PEq`, `POrd`, `Semigroup`, and `Monoid` instances for `PUnit`

    Module: `Plutarch.Unit`

    Added in: [#30](https://github.com/Plutonomicon/plutarch/pull/30)
  - UTF-8 encode/decode functions:- `pencodeUtf8`, `pdecodeUtf8`

    Module: `Plutarch.String`

    Added in: [#30](https://github.com/Plutonomicon/plutarch/pull/30)
  - `PIntegral` typeclass

    Module: `Plutarch.Integer`

    Added in: [#30](https://github.com/Plutonomicon/plutarch/pull/30)
  - `PIntegral` instance for `PInteger`

    Module: `Plutarch.Integer`

    Added in: [#30](https://github.com/Plutonomicon/plutarch/pull/30)
  - `PEq` instance for `PData` and `PAsData`

    Module: `Plutarch.Builtin`

    Added in: [#38](https://github.com/Plutonomicon/plutarch/pull/38)
  - Tracing functions:- `ptrace`, `ptraceIfFalse`, `ptraceIfTrue`, `ptraceError`

    Module: `Plutarch.Trace`

    Added in: [#39](https://github.com/Plutonomicon/plutarch/pull/39)
  - Builtin pair construction utility:- `ppairDataBuiltin `

    Module: `Plutarch.Builtin`

    Added in: [#50](https://github.com/Plutonomicon/plutarch/pull/50)
  - *Loads* of awesome list utilities thanks to [#63](https://github.com/Plutonomicon/plutarch/pull/63)!

    Module: `Plutarch.List`
  - `PIsData` instance for `PBool`

    Module: `Plutarch.Builtin`

    Added in: [#110](https://github.com/Plutonomicon/plutarch/pull/110)

  </details>
- Add `PIsData` for conversion between normal builtin types and their `Data` representation.

  Initially added in: [#31](https://github.com/Plutonomicon/plutarch/pull/31)
- Add `PAsData` for preserving more type information regarding `Data` encoded values.

  Initially added in: [#31](https://github.com/Plutonomicon/plutarch/pull/31)
- Add `PDataRepr` and related machinery to ergonomically work with `Constr` encoded data.

  There are also **generic derivers** (!!) to implement the related typeclasses for custom data types.

  Module: `Plutarch.DataRepr`

  Worked on through
  * [#31](https://github.com/Plutonomicon/plutarch/pull/31)
  * [#169](https://github.com/Plutonomicon/plutarch/pull/169)
  * [#173](https://github.com/Plutonomicon/plutarch/pull/173)
  * [#176](https://github.com/Plutonomicon/plutarch/pull/176)
  * [#171](https://github.com/Plutonomicon/plutarch/pull/171)
  * [#185](https://github.com/Plutonomicon/plutarch/pull/185)
- Add `PLift` and `PConstant` - enabling conversion between Plutarch terms and Haskell types.

  This comes with convenient derivers. See the relevant section on the guide.

  Module: `Plutarch.Lift`; another deriver within `Plutarch.DataRepr.Internal`

  Worked on through
  * [#62](https://github.com/Plutonomicon/plutarch/pull/62)
  * [#109](https://github.com/Plutonomicon/plutarch/pull/109)
  * [#130](https://github.com/Plutonomicon/plutarch/pull/130)
- Deprecate `punsafeConstant`. Use `pconstant` instead!
- Add `PIsList` & `PList` + instances for `PBuiltinList` - ergonomic list functions for all!

  Module: `Plutarch.List`; another instance in `Plutarch.Builtin`

  Added in: [#63](https://github.com/Plutonomicon/plutarch/pull/63)
- Mutually recursive data types, scott encoded records and more provided by `Plutarch.Rec`!

  Module: `Plutarch.Rec`

  Worked on through:
  * [#60](https://github.com/Plutonomicon/plutarch/pull/60)
  * [#114](https://github.com/Plutonomicon/plutarch/pull/114)
  * [#125](https://github.com/Plutonomicon/plutarch/pull/125)
  * [#175](https://github.com/Plutonomicon/plutarch/pull/175)
- The Plutus V1 ledger api types implemented in Plutarch!

  Module: `Plutarch.Api.V1`

  Worked on through:
  * [#66](https://github.com/Plutonomicon/plutarch/pull/66)
  * [#130](https://github.com/Plutonomicon/plutarch/pull/130)
  * [#151](https://github.com/Plutonomicon/plutarch/pull/151)
  * [#161](https://github.com/Plutonomicon/plutarch/pull/161)
  * [#163](https://github.com/Plutonomicon/plutarch/pull/163)
  * [#173](https://github.com/Plutonomicon/plutarch/pull/173)
- Implement Plutarch rational type.

  Module: `Plutarch.Rational`

  Added in: [#89](https://github.com/Plutonomicon/plutarch/pull/89)
- The `s` in `Term s _` has a fixed kind now. `data S`, `s :: S`

  Module: `Plutarch.Internal`

  Added in: [#115](https://github.com/Plutonomicon/plutarch/pull/115/files)
- Add `PType`, a synonym to `S -> Type` - i.e the kind of Plutarch types. In particular, the kind of the 3rd type parameter of `Term`.

  Module: `Plutarch.Internal`

  Added in: [#115](https://github.com/Plutonomicon/plutarch/pull/115/files)
- Convenient do syntax with `QualifiedDo` (!!!)

  Module: `Plutarch.Monadic`

  Added in: [#119](https://github.com/Plutonomicon/plutarch/pull/119)
- Add `DerivePNewtype` to derive common typeclasses for Plutarch newtypes.

  Module: `Plutarch`; `Plutarch.Builtin`; `Plutarch.Bool`; `Plutarch.Integer`

  Added in: [#151](https://github.com/Plutonomicon/plutarch/pull/151)
- A whole lot more exports from `Plutarch.Prelude`

  Module: `Plutarch.Prelude`

  Added in: [#181](https://github.com/Plutonomicon/plutarch/pull/181)
- Move out `punsafe*` functions from `Plutarch` and into `Plutarch.Unsafe`.
- Add generic deriving for `PlutusType` with scott encoding representation.

  Added in: [#189](https://github.com/Plutonomicon/plutarch/pull/189)

Thanks to:
- @blamario
- @emiflake
- @Geometer1729
- @kozross
- @L-as
- @MatthewCroughan
- @sergesku
- @srid
- @t1lde
- @TotallyNotChase

# 1.0.0

Somewhat stable release
