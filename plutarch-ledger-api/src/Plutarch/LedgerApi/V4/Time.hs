{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V4 module in plutus-ledger-api
module Plutarch.LedgerApi.V4.Time (
  PPosixTimeRange (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift (
  LiftError (CouldNotDecodeData),
  punsafeCoercePLifted,
 )
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V1.Time (PPosixTime)
import Plutarch.Prelude
import PlutusLedgerApi.V4 qualified as Plutus

-- | @since 3.8.0
data PPosixTimeRange (s :: S) = PPosixTimeRange
  { pposixTimeRange'fromInclusive :: Term s (PMaybeData PPosixTime)
  , pposixTimeRange'untilExclusive :: Term s (PMaybeData PPosixTime)
  }
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataRec PPosixTimeRange)

-- | @since 3.8.0
instance PLiftable PPosixTimeRange where
  type AsHaskell PPosixTimeRange = Plutus.POSIXTimeRange
  type PlutusRepr PPosixTimeRange = Plutus.Data
  haskToRepr = Plutus.toData
  reprToHask = maybe (Left CouldNotDecodeData) Right . Plutus.fromData
  plutToRepr t = fmap Plutus.List $ plutToReprUni @(PBuiltinList PData) $ punsafeCoercePLifted t
  reprToPlut = \case
    Plutus.List ell -> punsafeCoercePLifted (reprToPlutUni @(PBuiltinList PData) ell)
    _ -> error "Impossible: PPosixTimeRange is list-represented."

-- | @since 3.8.0
instance PTryFrom PData (PAsData PPosixTimeRange)
