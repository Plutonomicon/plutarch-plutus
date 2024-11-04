module Plutarch.Builtin.Unit (
  -- * Type
  PUnit (..),

  -- * Builtins
  pbuiltinChooseUnit,
) where

import Data.Kind (Type)
import Plutarch.Internal.Term (S, Term, punsafeBuiltin, (:-->))
import PlutusCore qualified as PLC

{- | The Plutus unit type.

@since WIP
-}
data PUnit (s :: S) = PUnit

-- | @since WIP
pbuiltinChooseUnit ::
  forall (a :: S -> Type) (s :: S).
  Term s (PUnit :--> a :--> a)
pbuiltinChooseUnit = punsafeBuiltin PLC.ChooseUnit
