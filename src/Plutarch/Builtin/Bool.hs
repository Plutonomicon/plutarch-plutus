module Plutarch.Builtin.Bool (
  -- * Type
  PBool (..),

  -- * Builtins
  pbuiltinIfThenElse,
) where

import Data.Kind (Type)
import Plutarch.Internal.Term (
  PDelayed,
  S,
  Term,
  punsafeBuiltin,
  (:-->),
 )
import PlutusCore qualified as PLC

{- | A Plutus boolean.

@since WIP
-}
data PBool (s :: S) = PTrue | PFalse
  deriving stock
    ( -- | @since WIP
      Show
    )

-- | @since WIP
pbuiltinIfThenElse ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBool :--> a :--> a :--> PDelayed a)
pbuiltinIfThenElse = punsafeBuiltin PLC.IfThenElse
