<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.PBool (pTheAnswer) where 
import Plutarch.Prelude
```

</p>
</details>

# `PBool`

Plutarch level boolean terms can be constructed using `pconstant True` and `pconstant False`.

```haskell
pTheAnswer :: forall s. Term s PInteger
pTheAnswer = pif (pconstant False) 7 42
```

You can combine Plutarch booleans terms using `#&&` and `#||`, which are synonyms to `&&` and `||`. These are Haskell level operators and therefore have short circuiting. 
If you don't need short circuiting, you can use the Plutarch level alternatives- `pand'` and `por'` respectively.

> Note: Be aware that there's a difference between `pif'` and `pif`, the former of which is strict (i.e. it evaluates both branches eagerly), the latter of which is lazy.
> `pif'` will be a Plutarch level function, whereas `pif` is Haskell level.

This is synonymous to Plutus Core [builtin boolean](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins-Internal.html#t:BuiltinBool).
