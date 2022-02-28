# `PEq` & `POrd`

Plutarch level equality is provided by the `PEq` typeclass:

```haskell
class PEq t where
  (#==) :: Term s t -> Term s t -> Term s PBool
```

`PInteger` implements `PEq` as you would expect. So you could do:

```haskell
1 #== 2
```

That would yield a `Term s PBool`, which you would probably use with `pif` (or similar).

Similarly, `POrd` emulates `Ord`:

```haskell
class POrd t where
  (#<=) :: Term s t -> Term s t -> Term s PBool
  (#<) :: Term s t -> Term s t -> Term s PBool
```

It works as you would expect:

```haskell
{-# LANGUAGE OverloadedStrings #-}

pif (1 #< 7) "indeed" "what"
```

evaluates to `"indeed"` - of type `Term s PString`.
