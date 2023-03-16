# Plutarch

[![Hercules-ci][Herc badge]][Herc link]
[![github pages link](https://img.shields.io/badge/docs-github.io-blueviolet)](https://plutonomicon.github.io/plutarch-plutus/)

[Herc badge]: https://img.shields.io/badge/ci--by--hercules-green.svg
[Herc link]: https://hercules-ci.com/github/Plutonomicon/plutarch

Plutarch is a typed eDSL in Haskell for writing efficient Plutus Core validators.

[**Continue to our website**](https://plutonomicon.github.io/plutarch-plutus/)

# Why Plutarch?
Plutarch written validators are often significantly more efficient than Plutus Tx written validators. With Plutarch, you have much more fine gained control of the Plutus Core you generate, without giving up any type information.

To put things into perspective, one validator script from a large production contract was rewritten in Plutarch, changed from Plutus Tx. Here's the comparison between the Plutarch script's execution cost compared to the Plutus Tx script's execution cost. These numbers were gathered by simulating the whole contract flow on a testnet:

| Version            | CPU         | Memory  | Script Size |
| ------------------ | ----------- | ------- | ----------- |
| PlutusTx (current) | 198,505,651 | 465,358 |  2013       |
| Plutarch           | 51,475,605  |  99,992 |  489        |

More benchmarks, with reproducible code, soon to follow.

# Installation
* Add this repo as a source repository package to your `cabal.project`.
* Add the `plutarch` package as a dependency to your cabal file.

# Benchmarks

See the [`plutarch-test`](./plutarch-test) for testing and golden files containing benchmarks and UPLCs.

# Usage
Read the [Plutarch guide](./plutarch-docs/README.md) to get started!

# Contributing
Contributions are more than welcome! Alongside the [User guide](#usage) above, you may also find the [Developers' guide](./docs/DEVGUIDE.md) useful for understanding the codebase.

## Developer communication channels

We're in the Plutonomicon Discord: https://discord.gg/722KnTC8jF

# Sponsors

This list is incomplete:
- Optim
- Liqwid
- Zamarad
- Ikigai
- SingularityNET
- KWARXs
- GeniusYield
- Noblesteed
- Xinsere
- A lot of other people probably.

Big thanks to everyone.

# License

```
Copyright (c) 2021-2022 Ardana Labs
Copyright (c) 2021-2022 Cardax B.V.
Copyright (c) 2021-2022 Minswap Team
Copyright (c) 2021-2022 Liqwid Labs
Copyright (c) 2021-2022 Platonic.Systems
Copyright (c) 2021-2022 MLabs
Copyright (c) 2022 Plutarch contributors and sponsors

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
