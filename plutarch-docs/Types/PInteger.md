<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.PInteger () where 
import Plutarch.Prelude ()
```

</p>
</details>

# `PInteger`

`Term s PInteger` has a convenient `Num` instance that allows you to construct Plutarch level integer terms from integer literals. 
It also means you have all the typical arithmetic operations available to you:

```hs
1 + 2
```

where `1` and `2` are `Term s PInteger`s.

Alongside `Num`, it also has a `PIntegral` instance, allowing you to use division, modulus etc.

It also has a `PEq` and `POrd` instance, allowing you to do Plutarch level equality and comparison.

It **does not** have a `PlutusType` instance.

> Note: be aware that GHC offers constant folding, i.e. at compile time expressions like `3 * 20000` will be folded to their result. This is important
> because if you were to instead use the Plutarch level function, the evaluation would instead be delayed until plutarch runtime increasing the script
> cost. 

This is synonymous to Plutus Core [builtin integer](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html#t:Integer).
