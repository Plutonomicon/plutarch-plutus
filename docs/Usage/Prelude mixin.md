# Using the Plutarch Prelude

Plutarch exports a Prelude (`Plutarch.Prelude`) that contains the most commonly used Plutarch functions, types and constructors.

The Plutarch Prelude `Plutarch.Prelude` has no overlap with `base` Prelude, which is the reason why you can use both of them together
without trouble. If you want to avoid importing `Plutarch.Prelude` in each of your modules, add the following to your `*.cabal` file:

```haskell
mixins:
  base hiding (Prelude)
  , plutarch-preludes (PPrelude as Prelude)
```
