# Contributing to Plutarch

Welcome! [Plutarch](./README.md) is a typed eDSL in Haskell for writing efficient Plutus Core
validators.

This document gets you from a fresh checkout to **a built repo with the test suite passing**. It is
deliberately step-by-step and assumes no prior experience with this codebase — only some Haskell.

Once you can build and test, see these companion docs:

- [`DEVGUIDE`](./plutarch-docs/src/DEVGUIDE.md) — code style, pre-commit conventions, and lower-level
  Plutus Core concepts.
- [`plutarch-docs/`](./plutarch-docs/) — the user guide (how to *write* Plutarch), rendered at
  <https://plutonomicon.github.io/plutarch-plutus/>.

---

## 1. Prerequisites

Plutarch depends on the Plutus/Cardano stack, which needs specific native crypto libraries and an
exact GHC version. The supported way to get all of this — matching what CI (Hercules) uses — is
**[Nix](https://nixos.org/download)** with flakes enabled.

- Install Nix (multi-user recommended): <https://nixos.org/download>.
- Enable flakes. Add this line to `~/.config/nix/nix.conf` (or `/etc/nix/nix.conf`):

  ```
  experimental-features = nix-command flakes
  ```

> **Why Nix?** The dev shell pins **GHC 9.8.4** and provides the patched `libsodium`, `secp256k1`,
> and `blst` that the Plutus stack links against. Building without Nix is possible but unsupported —
> see [§10](#10-building-without-nix-advanced--unsupported).

Optional but convenient: [`direnv`](https://direnv.net/). The repo ships an `.envrc`, so with direnv
installed the dev shell loads automatically when you `cd` into the directory.

## 2. Get the code

```sh
git clone https://github.com/Plutonomicon/plutarch-plutus.git
cd plutarch-plutus
```

(If you're working from a fork, clone your fork instead and add the upstream as a remote.)

## 3. Enter the development shell

From the repo root:

```sh
nix develop
```

This drops you into a shell with the full toolchain preconfigured:

- **GHC 9.8.4** and **cabal**
- **haskell-language-server** (editor integration), **hlint**, **fourmolu**, **cabal-fmt**
- `hspec-discover`, `markdown-unlit`, `mdbook`
- Git **pre-commit hooks** are installed automatically on entry.

If you use direnv instead, run `direnv allow` once; the shell then loads on `cd`.

> **Heads up:** the *first* `nix develop` downloads a large amount of prebuilt dependencies. This is
> a one-time cost — subsequent entries are fast.

## 4. Build

Inside the dev shell:

```sh
cabal build all
```

> **Heads up:** the *first* build compiles a large dependency tree (the Plutus stack), so it can take
> a while. Later incremental builds are quick.

## 5. Run the tests

```sh
cabal test all
```

The whole project has a single test suite — `tests` in the `plutarch-testlib` package (built on
`tasty`). You can run just it with:

```sh
cabal test plutarch-testlib
```

Some of these are **golden tests**: they compare output against reference files in
`plutarch-testlib/goldens/*.golden`. If you intentionally change generated output and need to update
those references, re-run with the accept flag:

```sh
cabal test plutarch-testlib --test-options='--accept'
```

Only accept goldens when you have reviewed the diff and the change is expected.

## 6. Try it in the REPL

To poke at Plutarch interactively:

```sh
cabal repl plutarch
```

For how to compile and evaluate a Plutarch term (`compile`, `evalWithArgsT`, etc.), see
[`plutarch-docs/src/Run.md`](./plutarch-docs/src/Run.md) and the guide's Overview.

## 7. Before you open a PR

- **Formatting & lint** run automatically through the pre-commit hooks installed by the dev shell
  (`fourmolu`, `cabal-fmt`, `hlint`, `typos`, and the Nix formatters). If a commit is blocked, let
  the hook reformat, then re-stage and commit again. You can run them across the tree at any time
  with `pre-commit run --all-files`.
- **Tests must pass:** run `cabal test all`.
- **Changelog:** if your change is user-facing, add an entry to the relevant `CHANGELOG.md`.
- **Which branch to target** and **code style** are covered in [`DEVGUIDE`](./plutarch-docs/src/DEVGUIDE.md).

> **Note:** older docs mention `./bin/format` and `cabal test -f development`. Both are obsolete —
> formatting is handled by the pre-commit hooks above, and there is no `development` flag. Use
> `cabal test all`.

## 8. Project layout

| Package | What it is |
| --- | --- |
| [`plutarch`](./plutarch.cabal) | The core eDSL: `Plutarch.Prelude`, terms, builtins, the `PlutusType`/`PLiftable` machinery, and `compile`. |
| [`plutarch-ledger-api`](./plutarch-ledger-api/) | Plutarch-level bindings for the Cardano ledger API types (V1/V2/V3 script contexts, `Value`, `Interval`, …). |
| [`plutarch-orphanage`](./plutarch-orphanage/) | Shared orphan instances (notably QuickCheck `Arbitrary`), isolated in one place to avoid orphan-instance warnings elsewhere. |
| [`plutarch-testlib`](./plutarch-testlib/) | Test/benchmark helpers **and** the project's single test suite, which exercises both `plutarch` and `plutarch-ledger-api`. |
| [`plutarch-docs`](./plutarch-docs/) | The mdbook user guide, plus a component that type-checks the guide's literate-Haskell snippets. |

## 9. Building the docs

The user guide is an [mdbook](https://rust-lang.github.io/mdBook/). From the dev shell:

```sh
cd plutarch-docs
mdbook serve .   # live-preview at http://localhost:3000
mdbook build .   # render static HTML into ./book
```

When you add a new *compilable* doc page, run `./createSymlinks` in `plutarch-docs/` and add the new
module to `plutarch-docs.cabal` under `other-modules` so it gets type-checked. See the "How to build
docs" section of [`DEVGUIDE`](./plutarch-docs/src/DEVGUIDE.md) for details.

## 10. Building without Nix (advanced / unsupported)

This path is **not supported** and you're on your own for troubleshooting, but in principle you need:

- **GHC 9.8.4** and a recent `cabal` (e.g. via [`ghcup`](https://www.haskell.org/ghcup/)).
- The Cardano Haskell Packages (CHAP) repository and the pinned `index-state` — both already declared
  in [`cabal.project`](./cabal.project). Run `cabal update` so cabal fetches the CHAP index.
- IntersectMBO's patched native C libraries — **libsodium**, **secp256k1**, and **blst** — installed
  and discoverable by `pkg-config`. See [`iohk-nix`](https://github.com/input-output-hk/iohk-nix) for
  the exact forks/versions; distro packages are often incompatible.

Then `cabal build all` / `cabal test all` as above. If the native libraries give you trouble, prefer
the Nix path in [§1](#1-prerequisites).

---

Questions or gaps in this guide? Open an issue, or ask in the Plutonomicon Discord:
<https://discord.gg/722KnTC8jF>.
