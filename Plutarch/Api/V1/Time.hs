{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Time (
  PPOSIXTime (PPOSIXTime),
  PPOSIXTimeRange,
) where

import Plutarch.Num (PNum)
import PlutusLedgerApi.V1 qualified as Plutus

import Plutarch.Api.V1.Interval (PInterval)
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Prelude
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)

newtype PPOSIXTime (s :: S)
  = PPOSIXTime (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PIntegral, PNum, PShow)
instance DerivePlutusType PPOSIXTime where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PPOSIXTime where type PLifted PPOSIXTime = Plutus.POSIXTime
deriving via
  (DerivePConstantViaNewtype Plutus.POSIXTime PPOSIXTime PInteger)
  instance
    PConstantDecl Plutus.POSIXTime

type PPOSIXTimeRange = PInterval PPOSIXTime

newtype Flip f a b = Flip (f b a) deriving stock (Generic)

instance PTryFrom PData (PAsData PPOSIXTime) where
  type PTryFromExcess PData (PAsData PPOSIXTime) = Flip Term PPOSIXTime
  ptryFrom' opq = runTermCont $ do
    (wrapped :: Term _ (PAsData PInteger), unwrapped :: Term _ PInteger) <-
      tcont $ ptryFrom @(PAsData PInteger) opq
    tcont $ \f -> pif (0 #<= unwrapped) (f ()) (ptraceError "ptryFrom(POSIXTime): must be positive")
    pure (punsafeCoerce wrapped, pcon $ PPOSIXTime unwrapped)
