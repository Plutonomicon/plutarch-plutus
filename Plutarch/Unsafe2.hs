{-# Options_GHC -w #-}

module Plutarch.Unsafe2 (PI.punsafeBuiltin) where

import Plutarch.Internal2 (Impl)
import qualified Plutarch.Internal2 as PI
-- import Plutarch.Internal.PlutusType (PInner)

import qualified Plutarch.Core as Core

{- |
  Unsafely coerce from the 'PInner' representation of a Term,
  assuming that the value is a safe construction of the Term.
-}
-- punsafeDowncast :: Impl (PInner a) -> Impl a
-- punsafeDowncast x = PI.punsafeCoerce x where

  -- punsafeDowncast' :: Core.PUntyped edsl
  --   => Core.IsPType edsl a
  --   => Core.IsPType edsl b
  --   => Core.Term edsl a -> Core.Term edsl b
  -- punsafeDowncast' x = Core.punsafeCoerce x

