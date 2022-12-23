<details>
<summary> imports </summary>
<p>

```haskell
module Plutarch.Docs.PList (pFe, pFeElim, pFeList) where 
import Plutarch.Prelude
```

</p>
</details>

# `PList`

Here's the [Scott encoded](./../Concepts/Data%20and%20Scott%20encoding.md#scott-encoding) cousin of `PBuiltinList`. What does that mean? Well, in practice, it just means that `PList` can contain _any arbitrary_ term - not just builtin types. `PList` also has a [`PListLike`](./../Typeclasses/PListLike.md) instance - so you won't be missing any of those utilities here!

`PList` also has a [`PlutusType`](../Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md) instance. You can construct a `PList` using `pcon` (but you should prefer using `pcons` from `PListLike`):

```haskell
pFeList :: forall s. Term s (PList PByteString)
pFeList = pcon $ PSCons (phexByteStr "fe") $ pcon PSNil
```

would yield a `PList PByteString` with one element - `0xfe`. Of course, you could have done that with `pcons # phexByteStr "fe" # pnil` instead!

You can also use `pmatch` to match on a list:

```haskell
pFe :: forall s. Term s PString
pFe = pmatch (pcon $ PSCons (phexByteStr "fe") $ pcon PSNil) $ \case
  PSNil -> "hey hey there's nothing here!"
  PSCons _ _ -> "oooo fancy!"
```

But you should prefer `pelimList` from `PListLike` instead:

```haskell
pFeElim :: forall s. Term s PString
pFeElim = pelimList (\_ _ -> "oooo fancy") "hey hey there's nothing here!" $ pcon $ PSCons (phexByteStr "fe") $ pcon PSNil
```
