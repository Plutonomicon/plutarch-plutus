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
bin/ghcid test
```

To run ghcid with development flag set:

```sh-session
bin/ghcid test:dev
```

Note: `cabal run` should be run inside `./plutarch-test` directory.

## The `development` flag

Plutarch has a `development` flag. Right now, the flag is used to control tracing functions, wherein turning on the flag will inject `Trace` instructions in the generated UPLC. 

Since this will impact the printTerm goldens in tests, we provide `plutarchDevFlagDescribe` that should be used everywhere in the test hierarchy where the immediate sub-tree of tests are known to use tracing functions (or any other development-flag-specific features to use tracing functions (or any other development-flag-specific features).

## Goldens

### Navigation

To quickly nagivate and preview the golden files in the terminal, run:

```sh-session
nix run nixpkgs#ranger -- ./plutarch-test/goldens/
```

Then hit `zv` to toggle on preview. Use `hjkl` to naviate.

### Reset

When writing tests you may want to clean up working copy goldens, and start from base. To do this, run:

```sh-session
rm plutarch-test/goldens/*.golden; git restore --source=HEAD --staged --worktree -- plutarch-test/goldens/
```