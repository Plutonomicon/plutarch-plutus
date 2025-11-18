module Plutarch.LedgerApi.Value.Lovelace (
  PLovelace (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude
import PlutusLedgerApi.V3 qualified as Plutus

-- | @since 2.2.0
newtype PLovelace (s :: S) = PLovelace (Term s PInteger)
  deriving stock
    ( -- | @since 2.2.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 2.2.0
      PIsData
    , -- | @since 2.2.0
      PEq
    , -- | @since 3.3.0
      POrd
    , -- | @since 2.2.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PLovelace)
  deriving
    ( -- | @since wip
      PValidateData
    )
    via (DeriveNewtypePValidateData PLovelace PInteger)

-- | @since 3.3.0
deriving via
  DeriveNewtypePLiftable PLovelace Plutus.Lovelace
  instance
    PLiftable PLovelace

-- | @since 3.4.0
instance PTryFrom PData (PAsData PLovelace)
