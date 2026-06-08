<details>
<summary> imports </summary>
<p>

```haskell
{-# OPTIONS_GHC -Wno-deprecations #-}
module Plutarch.Docs.PEqAndPOrd () where
```

</p>
</details>

# `PEq` & `POrd`

Plutarch level equality is provided by the `PEq` typeclass:

```hs
class PEq t where
  (#==) :: Term s t -> Term s t -> Term s PBool
```

`PInteger` implements `PEq` as you would expect. So you could do:

```hs
1 #== 2
```
That would yield a `Term s PBool`, which you would probably use with `pif` (or similar).

Similarly, `POrd` emulates `Ord`: 

```hs
-- The actual POrd has more methods, but these are the only required ones.
class PEq => POrd t where
  (#<) :: Term s t -> Term s t -> Term s PBool
  (#<=) :: Term s t -> Term s t -> Term s PBool
```

It works as you would expect:

```hs
pif (1 #< 7) "indeed" "what"
```

evaluates to `"indeed"` - of type `Term s PString`.

You can automatically derive `PEq` for anything with a `PlutusType` instance.

> Aside: `PEq` derivation for data encoded types uses "Data equality". It simply ensures the structure (as represented through [data encoding](../Concepts/DataAndScottEncoding.md#data-encoding)) of both values are _exactly_ the same. It does not take into account any custom `PEq` instances for the individual fields within.
