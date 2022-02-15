# `plutarch-benchmark`

**NOTE**: These benchmarks will soon be removed, to be replaced by the golden tests in `./plutarch-test`.

```sh
# If running from repo root:
cabal bench plutarch-benchmark
# Or, if running from this sub directory:
cabal bench
```

This will write the benchmark report to `bench.csv` as well as output a table view of the same.

## Benchmarking a commit
To run benchmarks on a particular commit,

```
nix run github:Plutonomicon/plutarch/<COMMIT-GOES-HERE>#benchmark
```

You can also emit a `.csv` file into stdout by passing the `--csv` flag:

```
nix run github:Plutonomicon/plutarch/<COMMIT-GOES-HERE>#benchmark -- --csv
```

## Benchmarking in CI

Note that you can also view these benchmarks on a per-commit basis by looking at Hercules CI logs. Go to the Hercules CI job run for a given commit, and navigate to the `checks.x86_64-linux.benchmark` page in the Attributes table, and then click on the "Log" header to view its output.

Additionally, a Hercules Effect to diff benchmarks between commits is created for PRs into staging. This is findable from the effects link on the PR.

## Diffing two `.csv` files

You can diff two previous result files using the `benchmark-diff` binary:

```
nix run .#benchmark-diff -- <old> <new>
```
