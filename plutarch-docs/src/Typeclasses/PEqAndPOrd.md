<details>
<summary> imports </summary>
<p>

```haskell
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Docs.PEqAndPOrd (PMaybe'(..)) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude
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

For SOP encoded types, you can easily derive `PEq` via generic deriving:

```haskell
data PMaybe' a s
  = PNothing'
  | PJust' (Term s a)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PEq)
  deriving (PlutusType) via (DeriveAsSOPStruct (PMaybe' a))
```

For data encoded types, you can derive `PEq` via their data representation:

```haskell
data PTriplet (a :: S -> Type) (s :: S)
  = PTriplet
      { ptriplet'a :: Term s (PAsData a)
      , ptriplet'b :: Term s (PAsData a)
      , ptriplet'c :: Term s (PAsData a)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq)
  deriving (PlutusType) via (DeriveAsDataStruct (PTriplet a))
```

> Aside: `PEq` derivation for data encoded types uses "Data equality". It simply ensures the structure (as represented through [data encoding](../Concepts/DataAndScottEncoding.md#data-encoding)) of both values are _exactly_ the same. It does not take into account any custom `PEq` instances for the individual fields within.
