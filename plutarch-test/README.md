# Plutarch tests and benchmarks

To run the tests using Nix:

```sh-session
$ cd $projectroot/plutarch-test
# Runs tests
$ nix run .#test-ghc9
# To run the above in GHC 8.10 instead:
$ nix run .#test-ghc810
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

## Goldens

### Navigation

To quickly navigate and preview the golden files in the terminal, run:

```sh-session
nix run nixpkgs#ranger -- ./plutarch-test/goldens/
```

Then hit `zv` to toggle on preview. Use `hjkl` to naviate.

### Reset

When writing tests you may want to clean up working copy goldens, and start from base. To do this, run:

```sh-session
rm plutarch-test/goldens/*.golden; git restore --source=HEAD --staged --worktree -- plutarch-test/goldens/
```
