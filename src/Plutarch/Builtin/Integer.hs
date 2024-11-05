module Plutarch.Builtin.Integer (
  -- * Type
  PInteger (..),

  -- * Builtins
  pbuiltinAddInteger,
  pbuiltinSubtractInteger,
  pbuiltinMultiplyInteger,
  pbuiltinDivideInteger,
  pbuiltinQuotientInteger,
  pbuiltinRemainderInteger,
  pbuiltinModInteger,
  pbuiltinExpModInteger,
  pbuiltinEqualsInteger,
  pbuiltinLessThanInteger,
  pbuiltinLessThanEqualsInteger,
) where

import Plutarch.Builtin.Bool (PBool)
import Plutarch.Builtin.Opaque (POpaque)
import Plutarch.Internal.Term (S, Term, punsafeBuiltin, (:-->))
import PlutusCore qualified as PLC

{- | A Plutus integer.

@since WIP
-}
newtype PInteger (s :: S) = PInteger (Term s POpaque)

-- | @since WIP
pbuiltinAddInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger)
pbuiltinAddInteger = punsafeBuiltin PLC.AddInteger

-- | @since WIP
pbuiltinSubtractInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger)
pbuiltinSubtractInteger = punsafeBuiltin PLC.SubtractInteger

-- | @since WIP
pbuiltinMultiplyInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger)
pbuiltinMultiplyInteger = punsafeBuiltin PLC.MultiplyInteger

-- | @since WIP
pbuiltinDivideInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger)
pbuiltinDivideInteger = punsafeBuiltin PLC.DivideInteger

-- | @since WIP
pbuiltinQuotientInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger)
pbuiltinQuotientInteger = punsafeBuiltin PLC.QuotientInteger

-- | @since WIP
pbuiltinRemainderInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger)
pbuiltinRemainderInteger = punsafeBuiltin PLC.RemainderInteger

-- | @since WIP
pbuiltinModInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger)
pbuiltinModInteger = punsafeBuiltin PLC.ModInteger

-- | @since WIP
pbuiltinExpModInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger :--> PInteger)
pbuiltinExpModInteger = punsafeBuiltin PLC.ExpModInteger

-- | @since WIP
pbuiltinLessThanInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PBool)
pbuiltinLessThanInteger = punsafeBuiltin PLC.LessThanInteger

-- | @since WIP
pbuiltinLessThanEqualsInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PBool)
pbuiltinLessThanEqualsInteger = punsafeBuiltin PLC.LessThanEqualsInteger

-- | @since WIP
pbuiltinEqualsInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PBool)
pbuiltinEqualsInteger = punsafeBuiltin PLC.EqualsInteger
