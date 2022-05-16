{-# LANGUAGE UndecidableInstances #-}

module Plutarch.NonZero (PNonZero, pnonZero, ptryNonZero) where

import Data.Functor.Const (Const)
import qualified GHC.Generics as GHC
import Generics.SOP (Generic, HasDatatypeInfo, I (I))

import Plutarch.Bool (PEq, POrd, pif, (#==))
import Plutarch.Builtin (PAsData, PData, PIsData, pdata)
import Plutarch.Integer (PInteger, PIntegral)
import Plutarch.Internal.Other (
  DerivePNewtype (DerivePNewtype),
  PlutusType,
  Term,
  pcon,
  phoistAcyclic,
  plam,
  plet,
  (#),
  type (:-->),
 )
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Reducible (Flip)
import Plutarch.Show (PShow)
import Plutarch.TermCont (runTermCont, tcont)
import Plutarch.Trace (ptraceError)
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'), ptryFrom)

newtype PNonZero s = PNonZero (Term s PInteger)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, HasDatatypeInfo, PShow)
  deriving (PlutusType, PIsData, PEq, POrd, PIntegral) via (DerivePNewtype PNonZero PInteger)

instance PTryFrom PInteger PNonZero where
  type PTryFromExcess PInteger PNonZero = Const ()
  ptryFrom' opq = runTermCont $ pure (ptryNonZero # opq, ())

instance PTryFrom PData (PAsData PNonZero) where
  type PTryFromExcess PData (PAsData PNonZero) = Flip Term PNonZero
  ptryFrom' opq = runTermCont $ do
    (_, i) <- tcont $ ptryFrom @(PAsData PInteger) opq
    res <- tcont . plet $ ptryNonZero # i
    pure (pdata res, res)

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
