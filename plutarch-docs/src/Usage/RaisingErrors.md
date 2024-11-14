<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.RaiseErrs (fails) where
import Plutarch.Prelude
```

</p>
</details>

# Raising errors

In PlutusTx, you'd signal validation failure with the [`error`](https://plutonomicon.github.io/plutarch-plutus/haddock/plutus-tx/html/PlutusTx-Builtins-Internal.html#v:error) function. You can do the same in Plutarch using `perror`.

```haskell
fails :: Term s (PData :--> PData :--> PData :--> PUnit)
fails = plam $ \_ _ _ -> perror
```
