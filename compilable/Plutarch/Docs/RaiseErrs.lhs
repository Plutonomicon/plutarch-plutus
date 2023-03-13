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

In PlutusTx, you'd signal validation failure with the [`error`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Prelude.html#v:error) function. You can do the same in Plutarch using `perror`.

```haskell
fails :: Term s (PData :--> PData :--> PData :--> PUnit)
fails = plam $ \_ _ _ -> perror
```
