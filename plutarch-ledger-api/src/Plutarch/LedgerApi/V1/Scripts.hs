{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V1 module in plutus-ledger-api
module Plutarch.LedgerApi.V1.Scripts (
  PScriptHash (..),
  PDatum (..),
  PRedeemer (..),
  PDatumHash (..),
  PRedeemerHash (..),
) where

import Plutarch.Builtin (PDataNewtype (PDataNewtype))
import Plutarch.Internal.Lift (DeriveDataPLiftable)
import Plutarch.LedgerApi.Utils (Mret)
import Plutarch.Prelude
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V3 qualified as Plutus

-- | @since 2.0.0
newtype PScriptHash (s :: S) = PScriptHash (Term s (PDataNewtype PByteString))
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
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PScriptHash where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  DeriveDataPLiftable PScriptHash Plutus.ScriptHash
  instance
    PLiftable PScriptHash

-- | @since 3.1.0
instance PTryFrom PData PScriptHash where
  type PTryFromExcess PData PScriptHash = Mret PScriptHash
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    tcont $ \f ->
      pif
        (plengthBS # unwrapped #== 28)
        (f ())
        (ptraceInfoError "ptryFrom(PScriptHash): must be 28 bytes long")
    pure (punsafeCoerce opq, pcon . PScriptHash . pcon . PDataNewtype . pdata $ unwrapped)

-- | @since 2.0.0
instance PTryFrom PData (PAsData PScriptHash) where
  type PTryFromExcess PData (PAsData PScriptHash) = Mret PScriptHash
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    tcont $ \f ->
      pif
        (plengthBS # unwrapped #== 28)
        (f ())
        (ptraceInfoError "ptryFrom(PScriptHash): must be 28 bytes long")
    pure (punsafeCoerce opq, pcon . PScriptHash . pcon . PDataNewtype . pdata $ unwrapped)

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
    , -- | @since 3.1.0
      PTryFrom PData
    )

-- | @since 2.0.0
instance DerivePlutusType PDatum where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  DeriveDataPLiftable PDatum Plutus.Datum
  instance
    PLiftable PDatum

-- | @since 3.1.0
instance PTryFrom PData (PAsData PDatum)

-- | @since 2.0.0
newtype PDatumHash (s :: S) = PDatumHash (Term s (PDataNewtype PByteString))
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
      POrd
    , -- | @since 2.0.0
      PShow
    , -- | @since 3.1.0
      PTryFrom PData
    )

-- | @since 2.0.0
instance DerivePlutusType PDatumHash where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  DeriveDataPLiftable PDatumHash Plutus.DatumHash
  instance
    PLiftable PDatumHash

-- | @since 3.1.0
instance PTryFrom PData (PAsData PDatumHash)

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
    , -- | @since 3.1.0
      PTryFrom PData
    )

-- | @since 2.0.0
instance DerivePlutusType PRedeemer where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  DeriveDataPLiftable PRedeemer Plutus.Redeemer
  instance
    PLiftable PRedeemer

-- | @since 3.1.0
instance PTryFrom PData (PAsData PRedeemer)

-- | @since 2.0.0
newtype PRedeemerHash (s :: S) = PRedeemerHash (Term s (PDataNewtype PByteString))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      POrd
    , -- | @since 3.1.0
      PShow
    , -- | @since 3.1.0
      PTryFrom PData
    )

-- | @since 3.1.0
instance DerivePlutusType PRedeemerHash where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  DeriveDataPLiftable PRedeemerHash Plutus.RedeemerHash
  instance
    PLiftable PRedeemerHash

-- | @since 3.1.0
instance PTryFrom PData (PAsData PRedeemerHash)
