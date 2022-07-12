{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.NonZero (PNonZero, pnonZero, ptryNonZero) where

import Data.Functor.Const (Const)
import GHC.Generics (Generic)

import Plutarch.Bool (PEq, POrd, pif, (#==))
import Plutarch.Builtin (PAsData, PData, PIsData, pdata)
import Plutarch.Integer (PInteger, PIntegral)

import Plutarch.Maybe (PMaybe (PJust, PNothing))

import Plutarch (
  DerivePlutusType (DPTStrat),
  PlutusType,
  PlutusTypeNewtype,
  Term,
  TermCont (runTermCont),
  pcon,
  phoistAcyclic,
  plam,
  plet,
  pthrow,
  pto,
  (#),
  (#$),
  type (:-->),
 )
import Plutarch.Num (PNum (pfromInteger, (#-)))
import Plutarch.Show (PShow)
import Plutarch.TermCont (tcont)
import Plutarch.Trace (ptraceError)
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'), ptryFrom)

newtype PNonZero s = PNonZero (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, POrd, PIntegral, PShow)
instance DerivePlutusType PNonZero where type DPTStrat _ = PlutusTypeNewtype

instance PNum PNonZero where
  x #- y = ptryNonZero #$ pto x #- pto y

  pfromInteger 0 = pthrow "pnonZero.pfromInteger: encountered 0"
  pfromInteger x = pcon $ PNonZero $ pfromInteger x

instance PTryFrom PInteger PNonZero where
  type PTryFromExcess PInteger PNonZero = Const ()
  ptryFrom' opq = runTermCont $ pure (ptryNonZero # opq, ())

newtype Flip f a b = Flip (f b a) deriving stock (Generic)

instance PTryFrom PData (PAsData PNonZero) where
  type PTryFromExcess PData (PAsData PNonZero) = Flip Term PNonZero
  ptryFrom' opq = runTermCont $ do
    (_, i) <- tcont $ ptryFrom @(PAsData PInteger) opq
    res <- tcont . plet $ ptryNonZero # i
    resData <- tcont . plet $ pdata res
    pure (resData, res)

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
