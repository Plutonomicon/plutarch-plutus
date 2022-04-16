{-# LANGUAGE AllowAmbiguousTypes #-}

module Plutarch.Unsafe (
  PI.punsafeBuiltin,
  PI.punsafeCoerce,
  PI.punsafeConstant,
  punsafeDowncast,
) where

import qualified Plutarch.Internal as PI
import Plutarch.Internal.Other (PInner, POpaque, Term)

{- |
  Unsafely coerce from the 'PInner' representation of a Term,
  assuming that the value is a safe construction of the Term.
-}
punsafeDowncast :: PInner b POpaque ~ a => Term s a -> Term s b
punsafeDowncast x = PI.punsafeCoerce x
