# Plutarch
Plutarch is a typed eDSL in Haskell for writing efficient Plutus Core validators.

# Why Plutarch?
Plutarch written validators are often significantly more efficient than Plutus Tx written validators. With Plutarch, you have much more fine gained control of the Plutus Core you generate, without giving up any type information.

To put things into perspective, one validator script from a large production contract was rewritten in Plutarch, changed from Plutus Tx. Here's the comparison between the Plutarch script's execution cost compared to the Plutus Tx script's execution cost. These numbers were gathered by simulating the whole contract flow on a testnet-
| Version            | CPU         | Memory  | Script Size |
| ------------------ | ----------- | ------- | ----------- |
| PlutusTx (current) | 198,505,651 | 465,358 |  2013       |
| Plutarch           | 51,475,605  |  99,992 |  489        |

More benchmarks, with reproducible code, soon to follow.

# Installation
* Add this repo as a source repository package to your `cabal.project`.
* Add the `plutarch` package as a dependency to your cabal file.

This package takes in a flag, `development`, that defaults to false. It's used to turn on "development mode". Following is a list of effects and their variations based on whether or not development mode is on.

| On | Off |
| -- | --- |
| Tracing functions from `Plutarch.Trace` log given message to the trace log. | Tracing functions from `Plutarch.Trace` do not log. They merely return their argument. |

You can turn on development mode by passing in the `development` flag in your `cabal.project` file-
```hs
package plutarch
  flags: +development
```

# Usage
Read the [Plutarch guide](./docs/GUIDE.md) to get started!

# Contributing
Contributions are more than welcome! Alongside the [User guide](#usage) above, you may also find the [Developers' guide](./docs/DEVGUIDE.md) useful for understanding the codebase.