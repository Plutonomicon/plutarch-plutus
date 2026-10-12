# Changelog for `plutarch-testlib`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## Unreleased

### Changed

- Fix typo in `propPTryFromRoundtrip`.

## 1.0.3

### Added

- Unit tests for `PValidateData` instances of plutarch-ledger-api types.
- `goldenEvalWithConfig`, which allows choosing compilation configuration for a
  golden test
- `Plutarch.Test.Methods` for checking if implementing an optional method of a
  Plutarch type class has a performance benefit or not
- Property tests for `Plutarch.LedgerApi.V3.Contexts` functions to ensure they
  behave consistently with their `plutus-ledger-api` equivalents.

## 1.0.2

### Added

- Tests for `DeriveAsTag` `PLiftable` lawfulness

## 1.0.1

### Removed

- Tests for `plutarch-orphanage`'s `MintValue`, as it no longer exists

## 1.0.0

### Added

- Migrated all tests from previous infrastructure to this library
