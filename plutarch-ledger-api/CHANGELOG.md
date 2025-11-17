# Changelog for `plutarch-ledger-api`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## Unreleased

### Added

* `SOP.Generic` derivations for `PDatum`, `PRedeemer`.

### Changed

* Replaced all internal uses of `pfixHoisted` with `pfix` for improved
  performance, at the cost of a negligible increase in script size in some
  cases.

### Removed

* `DerivePlutusType` instance for `PPosixTime`, `PDatum`, `PRedeemer` as 
  `DerivePlutusType` is deprecated now. 

## 3.5.0 -- 07/11/2025

### Added

* `pdifferenceWith` to `Plutarch.LedgerApi.AssocMap`
* `zipWithBuilder` and `zipWithDataBuilder` to `Plutarch.LedgerApi.AssocMap`
  along with the corresponding data types (`MergeHandler`, `BothPresentHandler`,
  and `OnePresentHandler`), enabling quick bootstrapping of custom zip functions
* `pmapMaybeWithKey` and `pmapMaybeDataWithKey` to `Plutarch.LedgerApi.AssocMap`
* `pemptyRawValue`, `pemptySortedValue` and `pemptyLedgerValue` functions to
  `Plutarch.LedgerApi.Value`
* `ptoLedgerValue` to convert `PSortedValue`s to `PLedgerValue`s
* Newtypes representing mint Values: `V1.MintValue` and `V3.MintValue`,
  along with their construction functions: `pempty`, `psingleton`, and
  `ptoMintValue`

### Changed

* `pfindOwnInput` has been renamed to `pfindInputByOutRef`
* `pfindInputByOutRef` (formerly `pfindOwnInput`) and `pgetContinuingOutputs`
  now both accept a builtin list of inputs wrapped in `PAsData`, enabling
  compatibility with `PTxInfo` inputs
* In `Plutarch.LedgerApi.AssocMap`:
    * `punionResolvingCollisionsWith` has been renamed to `punionWith`
    * `punionResolvingCollisionsWithData` has been renamed to `punionWithData`
    * `pzipWithDefaults`, `pdifference`, `pintersectionWith`, and
      `pintersectionWithData` functions now have more generic type signatures 
* In `Plutarch.LedgerApi.Value`:
    * `punionResolvingCollisionsWith` has been renamed to `punionWith`
    * `punionResolvingCollisionsWithData` has been renamed to `punionWithData`
* The following zip functions no longer have a `Commutativity` argument:
    * In `Plutarch.LedgerApi.AssocMap`: `punionWithData`, `punionWithData`,
      `pintersectionWith`, `pintersectionWithData`
    * In `Plutarch.LedgerApi.Value`: `punionWith`, `punionWithData`
* Refactored `PMap` with a `KeyGuarantees` type argument into two distinct
  newtypes over `PAssocMap`: `PUnsortedMap` and `PSortedMap`
* Removed `PValue` with type-level tags and introduced newtypes instead:
  `PRawValue`, `PSortedValue`, and `PLedgerValue`
* Removed `psingleton`, `psingletonData`, and `pconstantPositiveSingleton` in
  favor of new singleton functions for specific Value newtypes:
    * For `PRawValue`: `psingletonRawValue` and `psingletonRawValueData`
    * For `PSortedValue`: `psingletonSortedValue` and `psingletonSortedValueData`
    * For `PLedgerValue`: `psingletonLedgerValue` and `psingletonLedgerValueData`
* Replaced `pltPositive` and `pltNonZero` with a single function over
  `PSortedValue`s: `plt` 
* Replaced `pleqPositive` and `pleqNonZero` with a single function over
  `PSortedValue`s: `pleq`
* Updated `PTxInfo`, `PTxOut`, `PCommittee`, and `PGovernanceAction` definitions
  to use the newly introduced AssocMap and Value types

### Removed

* `Commutativity` data type
* `KeyGuarantees` and `AmountGuarantees` kinds
* `passertPositive`, `passertNonZero`, `pforgetPositive`, `pnormalize`,
  `padaOnlyValue`, and `pnoAdaValue` from `Plutarch.LedgerApi.Value`

## 3.4.0 -- 21-08-2025

### Removed

* `Mret`, as it wasn't used for anything

### Fixed

* Added missing `PTryFrom` instances for plutarch-ledger-api types

## 3.3.0 -- 01-29-2025

### Added

* `PCountable` and `PEnumerable` instances for `PPosixTime`
* `pposixTime` and `unPPosixTime`, to construct and deconstruct `PPosixTime`
  more conveniently
* `pisEmpty`, `pinclusiveLowerBound`, `pinclusiveUpperBound` to `Interval`
* `PAssetClass` in `LedgerApi.Value`, parallelling `AssetClass` from
  `plutus-ledger-api`
* `pmaybeDataToMaybe` to `Plutarch.LedgerApi.Utils`
* `PSBool` and functionality, originally from Plutarch, now in
  `Plutarch.LedgerApi.Utils`
* `POrd` instance for `PLovelace`
* `pltPositive`, `pltNonZero`, `pleqPositive`, `pleqNonZero` to replace the old
  `PPartialOrd` instances for `PValue`

### Changed

* All ledger types are now derived with new derivation tactics--namely ones found in `Plutarch.Repr.Data`.
* `PPosixTime` now permits negative values as a result of its operations
* `PEq`, `PPartialOrd` and `POrd` for `PLowerBound` and `PUpperBound` match the
  semantics of `plutus-ledger-api`'s equivalents for these types
* `V3.PTxInfo.mint` now has `NonZero` guarantee
* `V1.PTxInfo.mint` now has `NoGuarantees` guarantee
* `PPosixTime` now has a different set of arithmetical operations: currently,
  addition, subtraction, negation and scaling by both `PPositive` and
  `PInteger`.

### Fixed

* Bug in `PNum` instance for `PPosixTime`, where `#*` was implemented as `#+
  instead
* `pcontains` bug where open intervals would not be handled correctly (see [this
  issue](https://github.com/Plutonomicon/plutarch-plutus/issues/705))

## 3.2.1 -- 11-07-2024

### Changed

* `Plutarch.LedgerApi.Utils` identifiers are now re-exported from V1 and V2 as
  well

## 3.2.0 -- 05-07-2024

### Changed

* The default 'lift' for `Value` is now `PValue Unsorted NoGuarantees`

## 3.1.1 -- 27-06-2024

### Added

* V1 API support
* V2 API support

### Changed

* `Plutarch.LedgerApi` is now `Plutarch.LedgerApi.V3`

## 3.1.0 -- 07-06-2024

### Added

* Corresponding types to `PlutusLedgerApi.V3` new types:
    * `PTxCert`
    * `PLovelace`
    * `PDelegatee`
    * `PDRepCredential`
    * `PColdCommitteeCredential`
    * `PHotCommitteeCredential`
    * `PDRep`
    * `PVoter`
    * `PGovernanceActionId`
    * `PVote`
    * `PProposalProcedure`
    * `PGovernanceAction`
    * `PChangedParameters`
    * `PProposalProcedure`
    * `PScriptInfo`
    * `PCommittee`
* `PRationalData`, originally from `plutarch-extra`
* `PTryFrom` instances:
    * `PTryFrom PData (PAsData PLovelace)`
    * `PTryFrom PData PCurrencySymbol`
    * `PTryFrom PData PScriptHash`
    * `PTryFrom PData PTxId`
    * `PTryFrom PData (PAsData PTxId)`
    * `PTryFrom PData PTxOutRef`
    * `PTryFrom PData (PAsData PTxOutRef)`
    * `PTryFrom PData PDatumHash` instance
    * `PTryFrom PData (PAsData PDatumHash)`
    * `PTryFrom PData POutputDatum`
    * `PTryFrom PData (PAsData POutputDatum)`
    * `PTryFrom PData PTxInfo`
    * `PTryFrom PData (PAsData PTxInfo)`
    * `PTryFrom PData a => PTryFrom PData (PInterval a)`
    * `PTryFrom PData a => PTryFrom PData (PAsData (PInterval a))`
    * `PTryFrom PData a => PTryFrom PData (PLowerBound a)`
    * `PTryFrom PData a => PTryFrom PData (PAsData (PLowerBound a))`
    * `PTryFrom PData a => PTryFrom PData (PUpperBound a)`
    * `PTryFrom PData a => PTryFrom PData (PAsData (PUpperBound a))`
    * `PTryFrom PData a => PTryFrom PData (PExtended a)`
    * `PTryFrom PData a => PTryFrom PData (PAsData (PExtended a))`
    * `PTryFrom PData PRedeemer`
    * `PTryFrom PData (PAsData PRedeemer)`
    * `PTryFrom PData PPosixTime`
    * `PTryFrom PData PScriptContext`
    * `PTryFrom PData (PAsData PScriptContext)`
    * `PTryFrom PData PPubKeyHash`
    * `PTryFrom PData PTokenName`
* `PUnsafeLiftDecl (PExtended a)` instance
* `PConstantDecl (Extended a)` instance
* `PBoolData`, corresponding to PlutusTx's `Bool`

### Modified

* `Plutarch.Api` now targets V3 types and functionality (replacing V2):
    * `PTxInfo` now has new fields, as well as the `datum` field being renamed
      `data` to match upstream definitions
    * `PScriptPurpose` has two new constructors, and `PCertifying` has different
      fields
    * `PTxId` now guarantees 32 byte length
    * `PTxInfo`'s `mint` field now ensures that no zero entries are included
    * `PScriptContext` lost the `purpose` field, but gained `redeemer` and
       `scriptInfo` fields
* `PRedeemerHash` now has the same type classes instantiated as `PDatumHash` did

### Removed

* `PDCert`, as V3 no longer supports it

## 3.0.0 -- 10-04-2024

### Modified

* `plutarch-api` is renamed to `plutarch-ledger-api`, and any modules `Plutarch.Api.*` is renamed to `Plutarch.LedgerApi.*`

## 2.1.1 -- 22-01-2024

### Added

* `padaSymbol`, `padaToken`, `plookup`, `plookupData`, `plookupDataWith`,
  `psingleton`, `psingletonData`, `pfoldAt`, `pfoldAtData`, `pleftBiasedUnion`,
  `pdifference`, `punionResolvingCollisionsWithData`, `pany`, `pfindWithDefault`
  `pinsert`, `pdelete`, `pzipWithDefaults`, `pintersectionWith`,
  `pintersectionWithData`, `pforgetSorted`, `punsortedMapFromFoldable`,
  `psortedMapFromFoldable`, `pmapWithKey`, `ptryLookup`, `pkeysEqual`,
  `pkeysEqualUnsorted`, `pupdate`, `padjust`, `pfomdMapWithKey`,
  `pfoldlWithKey`, `pkeys`, `pkvPairKey`, `pkvPairValue`, `pkvPairLt` to `Plutarch.Api.AssocMap`
* `pconstantPositiveSingleton`, `pforgetSorted`, `psingleton`, `psingletonData`
  `pleftBiasedCurrencyUnion`, `pleftBiasedTokenUnion`,
  `punionResolvingCollisionsWithData`, `pvalueOf`, `passertSorted`,
  `padaSymbolData`, `plovelaceValueOf`, `pisAdaOnlyValue`, `padaOnlyValue`,
  `pnoAdaValue` to `Plutarch.Api.Value`
* `PTryFrom` instance for `PData (PAsData (PValue Unsorted NonZero))`
* `Plutarch.Api.Interval` module, with extra functionality originally from
  `plutarch-extra`
* `pfromDJust`, `pisDJust`, `pmaybeData`, `pdjust`, `pdnothing`,
  `pmaybeToMaybeData`, `passertDJust` in `Plutarch.Api` (ported from
  `plutarch-extra`)

### Modified

* `pparseDatum` now takes `PMap` as datum lookup table in order to be compatible with Ledger V2 types.
* `PInterval`, `PLowerBound`, `PUpperBound` and `PExtended` are now in
  `Plutarch.Api.Interval`, with re-exports from `Plutarch.Api`.

## 2.1.0 -- 22-01-2024

### Added

* `pgetContinuingOutputs`, `pfndOwnInput`, `pparseDatum` from `plutarch-extra`

### Removed

* `ptuple`, as it's unnecessary now

## 2.0.0 -- 17-01-2024

Initial version
