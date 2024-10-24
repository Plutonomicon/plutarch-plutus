# Changelog for `plutarch-orphanage`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## WIP

### Added

* `V3` version of `MintValue` that does not include zero Ada
* QuickCheck type class instances for `AssetClass`

### Changed

* `Arbitrary` instance for `POSIXTime` can now generate negative values as well.
  This does not affect `Interval POSIXTime`'s instance, as the formation rules
  assume non-negative interval endpoints.
* `V1.MintValue` and `V1.NonAdaValue` generators now include zero Ada entry

## 1.0.3 -- 04-07-2024

### Added

* `MintValue` as a helper for non-Ada `Value`s that are nonzero

### Fixed

* `FeeValue` now produces positive amounts only (as it should have from the
  get-go).

## 1.0.2 -- 27-06-2024

### Added

* Orphan instances for V1 and V2 ledger types

### Changed

* `UTxOValue` now ensures sortedness

## 1.0.1 -- 24-06-2024

### Added

* Orphan instances for V3 ledger types

## 1.0.0 -- 20-06-2024

Initial version
