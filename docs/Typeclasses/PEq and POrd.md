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

For scott encoded types, you can easily derive `PEq` via generic deriving:

```hs
import qualified GHC.Generics as GHC
import Generics.SOP

import Plutarch.Prelude

data PMaybe a s
  = PNothing
  | PJust (Term s a)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType, PEq)
```

For data encoded types, you can derive `PEq` and `POrd` via `PIsDataReprInstances`:

```hs
import qualified GHC.Generics as GHC
import Generics.SOP

import Plutarch.Prelude
import Plutarch.DataRepr

newtype PTriplet a s
  = PTriplet
      ( Term
          s
          ( PDataRecord
              '[ "x" ':= a
               , "y" ':= a
               , "z" ':= a
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PEq, POrd)
    via (PIsDataReprInstances (PTriplet a))
```

> Aside: `PEq` derivation for data encoded types uses "Data equality". It simply ensures the structure (as represented through [data encoding](../Concepts/Data%20and%20Scott%20encoding.md#data-encoding)) of both values are _exactly_ the same. It does not take into account any custom `PEq` instances for the individual fields within.
