# Plutarch
Plutarch is a typed eDSL in Haskell for writing efficient Plutus Core validators.

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
