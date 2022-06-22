{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Time (
  PPOSIXTime (PPOSIXTime),
  PPOSIXTimeRange,
) where

import Plutarch.Num (PNum)
import qualified PlutusLedgerApi.V1 as Plutus

import Plutarch.Api.V1.Interval (PInterval)
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Prelude

newtype PPOSIXTime (s :: S)
  = PPOSIXTime (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, POrd, PIntegral, PNum)
instance DerivePlutusType PPOSIXTime where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PPOSIXTime where type PLifted PPOSIXTime = Plutus.POSIXTime
deriving via
  (DerivePConstantViaNewtype Plutus.POSIXTime PPOSIXTime PInteger)
  instance
    PConstantDecl Plutus.POSIXTime

type PPOSIXTimeRange = PInterval PPOSIXTime
