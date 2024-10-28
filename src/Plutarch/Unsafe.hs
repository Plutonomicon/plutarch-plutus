module Plutarch.Unsafe (
  PI.punsafeBuiltin,
  PI.punsafeCoerce,
  punsafeDowncast,
) where

import Plutarch.Internal.PlutusType (PInner)
import Plutarch.Internal.Term (Term)
import Plutarch.Internal.Term qualified as PI

{- |
  Unsafely coerce from the 'PInner' representation of a Term,
  assuming that the value is a safe construction of the Term.
-}
punsafeDowncast :: Term s (PInner a) -> Term s a
punsafeDowncast = PI.punsafeCoerce
