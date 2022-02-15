# Plutarch tests and benchmarks

To run the tests using Nix:

```sh-session
# Runs tests
$ nix run .#test-ghc9-nodev
# To run the above in GHC 8.10 instead:
$ nix run .#test-ghc810-nodev
# Replace 'nodev' with 'dev' to run the tests with development flag set
```

To run the tests using ghcid (fit for writing tests):

```sh-session
$ ghcid -c 'cabal repl plutarch-test:exe:plutarch-test' -T Main.main
```

To run ghcid with development flag set:

```sh-session
$ vim cabal.project # And then uncomment the "flags: +developmenet" line.
$ ghcid -c 'cabal repl plutarch-test:exe:plutarch-test' -T Main.main
```

## Use as library

The `plutarch-test` library exposes the `Plutarch.Test` module for use in testing Plutarch code in user applications such as smart contracts. For benchmarking functions, see the `plutarch-benchmark` package.

## Goldens

To quickly nagivate and preview the golden files in the terminal, run:

```
nix run nixpkgs#ranger -- ./plutarch-test/goldens/
```

Then hit `zv` to toggle on preview. Use `hjkl` to naviate.