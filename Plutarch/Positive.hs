{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Positive (PPositive, ppositive, ptryPositive) where

import Data.Functor.Const (Const)
import Data.Text (pack)
import GHC.Generics (Generic)
import Plutarch (
  DerivePlutusType (DPTStrat),
  PlutusType,
  PlutusTypeNewtype,
  Term,
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
import Plutarch.Bool (PEq, POrd, PPartialOrd, pif, (#<=))
import Plutarch.Builtin (PAsData, PData, PIsData, pdata)
import Plutarch.Integer (PInteger, PIntegral)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Num (PNum (pfromInteger, (#-)))
import Plutarch.Show (PShow, pshow)
import Plutarch.Trace (ptraceInfoError)
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'), ptryFromInfo)

newtype PPositive s = PPositive (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PIntegral, PShow)
instance DerivePlutusType PPositive where type DPTStrat _ = PlutusTypeNewtype

instance PNum PPositive where
  x #- y = ptryPositive #$ pto x #- pto y

  pfromInteger x
    | x <= 0 =
        pthrow $
          "PPositive.pfromInteger: encountered non positive: " <> pack (show x)
    | otherwise = pcon $ PPositive $ pfromInteger x

instance PTryFrom PInteger PPositive where
  type PTryFromExcess PInteger PPositive = Const ()
  ptryFrom' opq _ f = f (ptryPositive # opq) ()

newtype Flip f a b = Flip (f b a) deriving stock (Generic)

instance PTryFrom PData (PAsData PPositive) where
  type PTryFromExcess PData (PAsData PPositive) = Flip Term PPositive
  ptryFrom' opq _ f = ptryFromInfo @(PAsData PInteger) opq "could not get PInteger as positive" $ \_ i ->
    plet (ptryPositive # i) $ \res ->
      plet (pdata res) $ \resData ->
        f resData res

-- | Build a 'PPositive' from a 'PInteger'. Yields 'PNothing' if argument is zero.
ppositive :: Term s (PInteger :--> PMaybe PPositive)
ppositive = phoistAcyclic $
  plam $ \i ->
    pif
      (i #<= 0)
      (pcon PNothing)
      $ pcon . PJust . pcon
      $ PPositive i

-- | Partial version of 'PPositive'. Errors if argument is zero.
ptryPositive :: Term s (PInteger :--> PPositive)
ptryPositive = phoistAcyclic $
  plam $ \i ->
    pif
      (i #<= 0)
      (ptraceInfoError $ "ptryPositive: building with non positive: " <> pshow i)
      $ pcon
      $ PPositive i
