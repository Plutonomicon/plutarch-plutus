<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.PTuple () where 
import Plutarch.Prelude ()
```

</p>
</details>

# `PTuple`

These are [data encoded](./../Concepts/Data%20and%20Scott%20encoding.md#data-encoding) pairs. You can build `PTuple`s using `ptuple`:

```hs
ptuple :: Term s (PAsData a :--> PAsData b :--> PTuple a b)
```

`PTuple` has a [`PDataFields`](./../Typeclasses/PIsDataRepr%20and%20PDataFields.md#all-about-extracting-fields) instance. As such, you can extract its fields using `pletFields` or `pfield`.

Since `PAsData (PBuiltinPair (PAsData a) (PAsData b))` and `PAsData (PTuple a b)` have the same representation - you can safely convert between them at no cost:

```hs
ptupleFromBuiltin :: Term s (PAsData (PBuiltinPair (PAsData a) (PAsData b))) -> Term s (PAsData (PTuple a b))

pbuiltinPairFromTuple :: Term s (PAsData (PTuple a b)) -> Term s (PAsData (PBuiltinPair (PAsData a) (PAsData b)))
```
