{-# LANGUAGE AllowAmbiguousTypes #-}

module Plutarch.Unsafe (
  PI.punsafeBuiltin,
  PI.punsafeCoerce,
  PI.punsafeConstant,
  punsafeFrom,
  punsafeFromOpaque,
) where

import Plutarch (PInner, POpaque, Term)
import qualified Plutarch.Internal as PI

{- |
  Unsafely coerce from an Opaque term to another type.
-}
punsafeFromOpaque :: Term s POpaque -> Term s a
punsafeFromOpaque = PI.punsafeCoerce

{- |
  Unsafely coerce from the 'PInner' representation of a Term,
  assuming that the value is a safe construction of the Term.
-}
punsafeFrom :: (forall b. Term s (PInner a b)) -> Term s a
punsafeFrom x = PI.punsafeCoerce x
