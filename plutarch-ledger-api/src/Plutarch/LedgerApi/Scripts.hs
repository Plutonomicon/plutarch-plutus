{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V1 module in plutus-ledger-api
module Plutarch.LedgerApi.Scripts (
  PScriptHash (..),
  PDatum (..),
  PRedeemer (..),
  PDatumHash (..),
  PRedeemerHash (..),
) where

import Plutarch.Builtin (PDataNewtype (PDataNewtype))
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData))
import Plutarch.LedgerApi.Utils (Mret)
import Plutarch.Lift (
  DerivePConstantViaBuiltin (DerivePConstantViaBuiltin),
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
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
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PScriptHash where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl PScriptHash where
  type PLifted PScriptHash = Plutus.ScriptHash

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.ScriptHash PScriptHash)
  instance
    PConstantDecl Plutus.ScriptHash

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

-- | @since 2.0.0
instance PUnsafeLiftDecl PDatum where
  type PLifted PDatum = Plutus.Datum

-- | @since 2.0.0
deriving via
  (DerivePConstantViaBuiltin Plutus.Datum PDatum PData)
  instance
    PConstantDecl Plutus.Datum

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
      PPartialOrd
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

-- | @since 2.0.0
instance PUnsafeLiftDecl PDatumHash where
  type PLifted PDatumHash = Plutus.DatumHash

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.DatumHash PDatumHash)
  instance
    PConstantDecl Plutus.DatumHash

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

-- | @since 2.0.0
instance PUnsafeLiftDecl PRedeemer where
  type PLifted PRedeemer = Plutus.Redeemer

-- | @since 2.0.0
deriving via
  (DerivePConstantViaBuiltin Plutus.Redeemer PRedeemer PData)
  instance
    PConstantDecl Plutus.Redeemer

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
      PPartialOrd
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

-- | @since 3.1.0
instance PUnsafeLiftDecl PRedeemerHash where
  type PLifted PRedeemerHash = Plutus.RedeemerHash

-- | @since 3.1.0
deriving via
  (DerivePConstantViaData Plutus.RedeemerHash PRedeemerHash)
  instance
    PConstantDecl Plutus.RedeemerHash

-- | @since 3.1.0
instance PTryFrom PData (PAsData PRedeemerHash)
