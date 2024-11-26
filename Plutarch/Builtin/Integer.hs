{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Builtin.Integer (
  -- * Type
  PInteger,

  -- * Functions
  pexpModInteger,
) where

import GHC.Generics (Generic)
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.Lift (DeriveBuiltinPLiftable, PLiftable, PLifted (PLifted))
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.Ord (POrd, PPartialOrd ((#<), (#<=)))
import Plutarch.Internal.Other (POpaque)
import Plutarch.Internal.PlutusType (DPTStrat, DerivePlutusType, PlutusType)
import Plutarch.Internal.Term (
  S,
  Term,
  (#),
  (:-->),
 )
import Plutarch.Unsafe (punsafeBuiltin)
import PlutusCore qualified as PLC

{- | A builtin Plutus integer.

@since WIP
-}
newtype PInteger s = PInteger (Term s POpaque)
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PInteger where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  (DeriveBuiltinPLiftable PInteger Integer)
  instance
    PLiftable PInteger

instance PEq PInteger where
  x #== y = punsafeBuiltin PLC.EqualsInteger # x # y

instance PPartialOrd PInteger where
  x #<= y = punsafeBuiltin PLC.LessThanEqualsInteger # x # y
  x #< y = punsafeBuiltin PLC.LessThanInteger # x # y

instance POrd PInteger

{- | Performs modulo exponentiation. More precisely, @pexpModInteger b e m@
performs @b@ to the power of @e@, modulo @m@. The result is always
non-negative.

= Note

This will error if the modulus is zero. When given a negative exponent, this
will try to find a modular multiplicative inverse, and will error if none
exists.

@since WIP
-}
pexpModInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger :--> PInteger)
pexpModInteger = punsafeBuiltin PLC.ExpModInteger
