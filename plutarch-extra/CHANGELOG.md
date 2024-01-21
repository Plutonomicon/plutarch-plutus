# Changelog for `plutarch-extra`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## 1.3.1 -- 22-01-2024

### Changed

* `Plutarch.Extra.Interval`, `Plutarch.Extra.Map`, and `Plutarch.Extra.Maybe` use 
   `plutarch-api` definitions

### Removed

* `Plutarch.Extra.Api` as these functions are now in `plutarch-api`

## 1.2.1 -- 17-01-2024

### Fixed

* `Plutarch.Extra.RationalData.PRationalData` instances of `PPartialOrd` and
  `POrd` ([issue
  618](https://github.com/Plutonomicon/plutarch-plutus/issues/618)) 
