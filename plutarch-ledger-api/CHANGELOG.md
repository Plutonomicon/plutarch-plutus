# Changelog for `plutarch-ledger-api-v2`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

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
