module Plutarch.Builtin.Bool (
  -- * Type
  PBool (..),

  -- * Builtins
  pbuiltinIfThenElse,
) where

import Data.Kind (Type)
import Plutarch.Internal.Term (
  S,
  Term,
  pforce,
  phoistAcyclic,
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

{- | A strict if-then-else; both branches get evaluated regardless.

@since WIP
-}
pbuiltinIfThenElse ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBool :--> a :--> a :--> a)
pbuiltinIfThenElse = phoistAcyclic $ pforce $ punsafeBuiltin PLC.IfThenElse
