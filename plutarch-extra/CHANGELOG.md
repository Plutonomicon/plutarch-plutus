# Changelog for `plutarch-extra`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## 1.4.0 -- 07-06-2024

### Removed

* `PRationalData` module (now in `plutarch-ledger-api`)

## 1.3.1 -- 22-01-2024

### Changed

* `Plutarch.Extra.Map`, and `Plutarch.Extra.Maybe` use `plutarch-ledger-api` definitions

### Removed

* `Plutarch.Extra.Api` as these functions are now in `plutarch-ledger-api`
* `Plutarch.Extra.Interval`, as these functions are now in `plutarch-ledger-api`
* `Plutarch.Extra.Map`, as these functions are now in `plutarch-ledger-api`
* `pfromDJust`, `pisDJust`, `pmaybeData`, `pdjust`, `pdnothing`,
  `pmaybeToMaybeData`, `passertDJust` from `Plutarch.Extra.Maybe` (these are now
  in `plutarch-ledger-api`)

## 1.2.1 -- 17-01-2024

### Fixed

* `Plutarch.Extra.RationalData.PRationalData` instances of `PPartialOrd` and
  `POrd` ([issue
  618](https://github.com/Plutonomicon/plutarch-plutus/issues/618)) 
