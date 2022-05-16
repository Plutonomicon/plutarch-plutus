{-# LANGUAGE UndecidableInstances #-}

module Plutarch.NonZero (PNonZero, pnonZero, ptryNonZero) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, HasDatatypeInfo, I (I))
import Plutarch.Bool (PEq, POrd, pif, (#==))
import Plutarch.Builtin (PIsData)
import Plutarch.Integer (PInteger, PIntegral)
import Plutarch.Internal.Other (
  DerivePNewtype (DerivePNewtype),
  PlutusType,
  Term,
  pcon,
  phoistAcyclic,
  plam,
  type (:-->),
 )
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Show (PShow)
import Plutarch.Trace (ptraceError)

newtype PNonZero s = PNonZero (Term s PInteger)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, HasDatatypeInfo, PShow)
  deriving (PlutusType, PIsData, PEq, POrd, PIntegral) via (DerivePNewtype PNonZero PInteger)

-- | Build a 'PNonZero' from a 'PInteger'. Yields 'PNothing' if argument is zero.
pnonZero :: Term s (PInteger :--> PMaybe PNonZero)
pnonZero = phoistAcyclic $
  plam $ \i ->
    pif
      (i #== 0)
      (pcon PNothing)
      $ pcon . PJust . pcon $ PNonZero i

-- | Partial version of 'pnonZero'. Errors if argument is zero.
ptryNonZero :: Term s (PInteger :--> PNonZero)
ptryNonZero = phoistAcyclic $
  plam $ \i ->
    pif
      (i #== 0)
      (ptraceError "pnonZero: building with 0")
      $ pcon $ PNonZero i
