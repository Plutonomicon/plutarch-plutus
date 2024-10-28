{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Integer (
  -- * Type class
  PIntegral (..),
) where

import Plutarch.Bool (POrd, PPartialOrd, (#<), (#<=))
import Plutarch.Internal.Builtin (PInteger, pif, plam, pto)
import Plutarch.Internal.Eq ((#==))
import Plutarch.Internal.PlutusType (PInner)
import Plutarch.Internal.Term (
  Term,
  phoistAcyclic,
  (#),
  (:-->),
 )
import Plutarch.Lift (pconstant)
import Plutarch.Num (PNum, pabs, pfromInteger, pnegate, psignum, (#*), (#+), (#-))
import Plutarch.Unsafe (punsafeBuiltin, punsafeDowncast)
import PlutusCore qualified as PLC

class PIntegral a where
  pdiv :: Term s (a :--> a :--> a)
  default pdiv :: PIntegral (PInner a) => Term s (a :--> a :--> a)
  pdiv = phoistAcyclic $ plam $ \x y -> punsafeDowncast $ pdiv # pto x # pto y
  pmod :: Term s (a :--> a :--> a)
  default pmod :: PIntegral (PInner a) => Term s (a :--> a :--> a)
  pmod = phoistAcyclic $ plam $ \x y -> punsafeDowncast $ pmod # pto x # pto y
  pquot :: Term s (a :--> a :--> a)
  default pquot :: PIntegral (PInner a) => Term s (a :--> a :--> a)
  pquot = phoistAcyclic $ plam $ \x y -> punsafeDowncast $ pquot # pto x # pto y
  prem :: Term s (a :--> a :--> a)
  default prem :: PIntegral (PInner a) => Term s (a :--> a :--> a)
  prem = phoistAcyclic $ plam $ \x y -> punsafeDowncast $ prem # pto x # pto y

instance PIntegral PInteger where
  pdiv = punsafeBuiltin PLC.DivideInteger
  pmod = punsafeBuiltin PLC.ModInteger
  pquot = punsafeBuiltin PLC.QuotientInteger
  prem = punsafeBuiltin PLC.RemainderInteger

instance PPartialOrd PInteger where
  x #<= y = punsafeBuiltin PLC.LessThanEqualsInteger # x # y
  x #< y = punsafeBuiltin PLC.LessThanInteger # x # y

instance POrd PInteger

instance PNum PInteger where
  x #+ y = punsafeBuiltin PLC.AddInteger # x # y
  x #- y = punsafeBuiltin PLC.SubtractInteger # x # y
  x #* y = punsafeBuiltin PLC.MultiplyInteger # x # y
  pabs = phoistAcyclic $ plam \x -> pif (x #<= -1) (negate x) x
  pnegate = phoistAcyclic $ plam (0 #-)
  psignum = plam \x ->
    pif
      (x #== 0)
      0
      $ pif
        (x #<= 0)
        (-1)
        1
  pfromInteger = pconstant
