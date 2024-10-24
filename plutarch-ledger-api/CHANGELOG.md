# Changelog for `plutarch-ledger-api-v2`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## WIP

### Added

* `PCountable` and `PEnumerable` instances for `PPosixTime`
* `pposixTime` and `unPPosixTime`, to construct and deconstruct `PPosixTime`
  more conveniently
* `pisEmpty`, `pinclusiveLowerBound`, `pinclusiveUpperBound` to `Interval`
* `PAssetClass` in `LedgerApi.Value`, parallelling `AssetClass` from
  `plutus-ledger-api`

### Changed

* `PPosixTime` now permits negative values as a result of its operations
* `PEq`, `PPartialOrd` and `POrd` for `PLowerBound` and `PUpperBound` match the
  semantics of `plutus-ledger-api`'s equivalents for these types
* `V3.PTxInfo.mint` now has `NonZero` guarantee
* `V1.PTxInfo.mint` now has `NoGuarantees` guarantee

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
