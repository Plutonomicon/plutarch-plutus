{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Time (
  PPOSIXTime,
  PPOSIXTimeRange,
) where

import qualified Plutus.V1.Ledger.Api as Plutus

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
  deriving (PlutusType, PIsData, PEq, POrd, PIntegral) via (DerivePNewtype PPOSIXTime PInteger)

instance PUnsafeLiftDecl PPOSIXTime where type PLifted PPOSIXTime = Plutus.POSIXTime
deriving via
  (DerivePConstantViaNewtype Plutus.POSIXTime PPOSIXTime PInteger)
  instance
    PConstantDecl Plutus.POSIXTime

type PPOSIXTimeRange = PInterval PPOSIXTime
