{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V1 module in plutus-ledger-api
module Plutarch.LedgerApi.V1.Scripts (
  PScriptHash (..),
  PDatum (..),
  PRedeemer (..),
  PDatumHash (..),
  PRedeemerHash (..),
) where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude
import PlutusLedgerApi.V3 qualified as Plutus
import PlutusTx.Builtins.Internal qualified as PlutusTx

-- | @since 2.0.0
newtype PScriptHash (s :: S) = PScriptHash (Term s PByteString)
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PScriptHash)

-- | @since 3.4.0
instance PTryFrom PData (PAsData PScriptHash) where
  ptryFrom' opq = runTermCont $ do
    let bs = pasByteStr # opq
    pure (pdata . pcon . PScriptHash $ bs, ())

-- | @since 3.3.0
instance PLiftable PScriptHash where
  type AsHaskell PScriptHash = Plutus.ScriptHash
  type PlutusRepr PScriptHash = ByteString
  {-# INLINEABLE haskToRepr #-}
  haskToRepr (Plutus.ScriptHash (PlutusTx.BuiltinByteString str)) = str
  {-# INLINEABLE reprToHask #-}
  reprToHask = Right . Plutus.ScriptHash . PlutusTx.BuiltinByteString
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = reprToPlutUni
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = plutToReprUni

-- | @since 2.0.0
newtype PDatum (s :: S) = PDatum (Term s PData)
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PDatum where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PDatum Plutus.Datum
  instance
    PLiftable PDatum

-- | @since 3.4.0
instance PTryFrom PData (PAsData PDatum)

-- | @since 2.0.0
newtype PDatumHash (s :: S) = PDatumHash (Term s PByteString)
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PDatumHash)

-- | @since 3.4.0
instance PTryFrom PData (PAsData PDatumHash)

-- | @since 3.3.0
instance PLiftable PDatumHash where
  type AsHaskell PDatumHash = Plutus.DatumHash
  type PlutusRepr PDatumHash = ByteString
  {-# INLINEABLE haskToRepr #-}
  haskToRepr (Plutus.DatumHash (PlutusTx.BuiltinByteString str)) = str
  {-# INLINEABLE reprToHask #-}
  reprToHask = Right . Plutus.DatumHash . PlutusTx.BuiltinByteString
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = reprToPlutUni
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = plutToReprUni

-- | @since 2.0.0
newtype PRedeemer (s :: S) = PRedeemer (Term s PData)
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 3.4.0
instance PTryFrom PData (PAsData PRedeemer)

-- | @since 2.0.0
instance DerivePlutusType PRedeemer where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PRedeemer Plutus.Redeemer
  instance
    PLiftable PRedeemer

-- | @since 2.0.0
newtype PRedeemerHash (s :: S) = PRedeemerHash (Term s PByteString)
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      POrd
    , -- | @since 3.1.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PRedeemerHash)

-- | @since 3.4.0
instance PTryFrom PData (PAsData PRedeemerHash)

-- | @since 3.3.0
instance PLiftable PRedeemerHash where
  type AsHaskell PRedeemerHash = Plutus.RedeemerHash
  type PlutusRepr PRedeemerHash = ByteString
  {-# INLINEABLE haskToRepr #-}
  haskToRepr (Plutus.RedeemerHash (PlutusTx.BuiltinByteString str)) = str
  {-# INLINEABLE reprToHask #-}
  reprToHask = Right . Plutus.RedeemerHash . PlutusTx.BuiltinByteString
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = reprToPlutUni
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = plutToReprUni
