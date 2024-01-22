# Changelog for `plutarch-test`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## 1.2.1 -- 22-01-2024

### Changed

* `ApiSpec`, `FieldSpec`, `IntervalSpec`, `LiftSpec`, `MaybeSpec` `PIsDataSpec`, 
  `POrdSpec`, `PlutusTypeSpec` and `ScriptsSpec` use `plutarch-api` definitions

### Removed

* Tests for `pbuiltinPairFromTuple` and `ptupleFromBuiltin`, as they're no
  longer needed
* Isomorphism test for `PTuple`, as it's no longer needed
