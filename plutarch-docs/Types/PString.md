<details>
<summary> imports </summary>
<p>

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Plutarch.Docs.PString (pfoo) where 
import Plutarch.Prelude
```

</p>
</details>

# `PString`

`Term s PString` has a `IsString` instance. This allows you to make Plutarch level string terms from regular string literals, provided you have `OverloadedStrings` turned on.

```haskell
pfoo :: forall s. Term s PString
pfoo = "foo"
```

It also has a `PEq` instance. And its terms have `Semigroup` and `Monoid` instances - which work the way you would expect.

It **does not** have a `PlutusType` instance.

This is synonymous to Plutus Core [builtin string](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html#t:BuiltinString) (actually Text).
