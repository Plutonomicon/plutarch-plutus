<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.PBuiltinPair () where 
import Plutarch.Prelude ()
```

</p>
</details>

# `PBuiltinPair`

Much like in the case of builtin lists, you'll just be working with builtin functions (or rather, Plutarch synonyms to builtin functions) here. You can find everything about 
that in [builtin-pairs](https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-pairs.md). Feel free to only read the `Plutarch` examples.

In particular, you can deconstruct `PBuiltinPair` using `pfstBuiltin` and `psndBuiltin`. You can build `PBuiltinPair (PAsData a) (PAsData b)` terms with `ppairDataBuiltin`:

```hs
ppairDataBuiltin :: Term s (PAsData a :--> PAsData b :--> PBuiltinPair (PAsData a) (PAsData b))
```

It's also helpful to note that `PAsData (PBuiltinPair (PAsData a) (PAsData b))` and `PAsData (PTuple a b)` actually have the same representation under the hood. See [`PTuple`](./PTuple.md).
