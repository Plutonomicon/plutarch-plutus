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

## The `developmenmt` flag

Plutarch has a `development` flag. Right now, the flag is used to control tracing functions, wherein turning on the flag will inject `Trace` instructions in the generated UPLC. 

Since this will impact the printTerm goldens in tests, we provide `plutarchDevFlagDescribe` that should be used everywhere in the test hierarchy where the immediate sub-tree of tests are known to use tracing functions (or any other development-flag-specific featuresto use tracing functions (or any other development-flag-specific features).

## Goldens

To quickly nagivate and preview the golden files in the terminal, run:

```
nix run nixpkgs#ranger -- ./plutarch-test/goldens/
```

Then hit `zv` to toggle on preview. Use `hjkl` to naviate.
