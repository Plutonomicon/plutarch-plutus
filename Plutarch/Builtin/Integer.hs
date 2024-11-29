{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Builtin.Integer (
  -- * Type
  PInteger,

  -- * Functions
  pexpModInteger,
) where

import GHC.Generics (Generic)
import Plutarch.Builtin.Bool (pcond, pif)
import Plutarch.Internal.Eq (PEq, (#==))
import Plutarch.Internal.IsData
import Plutarch.Internal.Lift (DeriveBuiltinPLiftable, PLiftable, PLifted (PLifted), pconstant)
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.Numeric (PIntegral, PNum, pabs, pdiv, pfromInteger, pmod, pnegate, pquot, prem, psignum, (#*), (#+), (#-))
import Plutarch.Internal.Ord (POrd, (#<), (#<=))
import Plutarch.Internal.Other (POpaque)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (DPTStrat, DerivePlutusType, PlutusType)
import Plutarch.Internal.Term (S, Term, phoistAcyclic, (#), (:-->))
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

instance PNum PInteger where
  {-# INLINEABLE (#+) #-}
  x #+ y = punsafeBuiltin PLC.AddInteger # x # y
  {-# INLINEABLE (#-) #-}
  x #- y = punsafeBuiltin PLC.SubtractInteger # x # y
  {-# INLINEABLE (#*) #-}
  x #* y = punsafeBuiltin PLC.MultiplyInteger # x # y
  {-# INLINEABLE pabs #-}
  pabs = phoistAcyclic $ plam \x -> pif (x #<= -1) (negate x) x
  {-# INLINEABLE pnegate #-}
  pnegate = phoistAcyclic $ plam (0 #-)

  --  Note from Koz (27/11/2024): we don't hoist this, and we use #<= instead of
  --  (the arguably more correct) #< here, because both of these lead to perf
  --  losses and size increases. Don't ask why, I don't make the rules.
  {-# INLINEABLE psignum #-}
  psignum = plam $ \x ->
    pcond
      [ (x #== 0, 0)
      , (x #<= 0, -1)
      ]
      1
  {-# INLINEABLE pfromInteger #-}
  pfromInteger = pconstant

instance PIntegral PInteger where
  {-# INLINEABLE pdiv #-}
  pdiv = punsafeBuiltin PLC.DivideInteger
  {-# INLINEABLE pmod #-}
  pmod = punsafeBuiltin PLC.ModInteger
  {-# INLINEABLE pquot #-}
  pquot = punsafeBuiltin PLC.QuotientInteger
  {-# INLINEABLE prem #-}
  prem = punsafeBuiltin PLC.RemainderInteger

-- | @since WIP
instance PEq PInteger where
  {-# INLINEABLE (#==) #-}
  x #== y = punsafeBuiltin PLC.EqualsInteger # x # y

instance POrd PInteger where
  {-# INLINEABLE (#<=) #-}
  x #<= y = punsafeBuiltin PLC.LessThanEqualsInteger # x # y
  {-# INLINEABLE (#<) #-}
  x #< y = punsafeBuiltin PLC.LessThanInteger # x # y

instance PIsData PInteger where
  pfromDataImpl x = punsafeBuiltin PLC.UnIData # pforgetData x
  pdataImpl x = punsafeBuiltin PLC.IData # x
