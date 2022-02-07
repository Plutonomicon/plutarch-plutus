{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Time (
  PPOSIXTime,
  PPOSIXTimeRange,
  PDiffMilliSeconds (PDiffMilliSeconds),
) where

import qualified Plutus.V1.Ledger.Time as PlutusTime

import Plutarch.Api.V1.Interval (PInterval)
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Prelude

newtype PPOSIXTime (s :: S)
  = PPOSIXTime (Term s PInteger)
  deriving (PlutusType, PIsData, PEq, POrd, PIntegral) via (DerivePNewtype PPOSIXTime PInteger)

instance PUnsafeLiftDecl PPOSIXTime where type PLifted PPOSIXTime = PlutusTime.POSIXTime
deriving via
  (DerivePConstantViaNewtype PlutusTime.POSIXTime PPOSIXTime PInteger)
  instance
    (PConstant PlutusTime.POSIXTime)

type PPOSIXTimeRange = PInterval PPOSIXTime

newtype PDiffMilliSeconds (s :: S)
  = PDiffMilliSeconds (Term s PInteger)
  deriving (PlutusType, PIsData, PEq, POrd, PIntegral) via (DerivePNewtype PDiffMilliSeconds PInteger)

instance PUnsafeLiftDecl PDiffMilliSeconds where type PLifted PDiffMilliSeconds = PlutusTime.DiffMilliSeconds
deriving via
  (DerivePConstantViaNewtype PlutusTime.DiffMilliSeconds PDiffMilliSeconds PInteger)
  instance
    (PConstant PlutusTime.DiffMilliSeconds)
