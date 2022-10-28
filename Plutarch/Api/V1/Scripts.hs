{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Scripts (
  -- * Plutus API Types
  PDatum (PDatum),
  PDatumHash (PDatumHash),
  PRedeemer (PRedeemer),
  PRedeemerHash (PRedeemerHash),
  PScriptHash (PScriptHash),
) where

import PlutusLedgerApi.V1 qualified as Plutus

import Plutarch.Lift (
  DerivePConstantViaBuiltin (DerivePConstantViaBuiltin),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Prelude
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)

newtype PDatum (s :: S) = PDatum (Term s PData)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)
instance DerivePlutusType PDatum where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PDatum where type PLifted PDatum = Plutus.Datum
deriving via (DerivePConstantViaBuiltin Plutus.Datum PDatum PData) instance PConstantDecl Plutus.Datum

newtype PRedeemer (s :: S) = PRedeemer (Term s PData)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)
instance DerivePlutusType PRedeemer where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PRedeemer where type PLifted PRedeemer = Plutus.Redeemer
deriving via (DerivePConstantViaBuiltin Plutus.Redeemer PRedeemer PData) instance PConstantDecl Plutus.Redeemer

newtype PDatumHash (s :: S) = PDatumHash (Term s PByteString)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow)
instance DerivePlutusType PDatumHash where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PDatumHash where type PLifted PDatumHash = Plutus.DatumHash
deriving via (DerivePConstantViaBuiltin Plutus.DatumHash PDatumHash PByteString) instance PConstantDecl Plutus.DatumHash

newtype PRedeemerHash (s :: S) = PRedeemerHash (Term s PByteString)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow)
instance DerivePlutusType PRedeemerHash where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PRedeemerHash where type PLifted PRedeemerHash = Plutus.RedeemerHash
deriving via
  (DerivePConstantViaBuiltin Plutus.RedeemerHash PRedeemerHash PByteString)
  instance
    PConstantDecl Plutus.RedeemerHash

newtype PScriptHash (s :: S) = PScriptHash (Term s PByteString)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow)
instance DerivePlutusType PScriptHash where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PScriptHash where type PLifted PScriptHash = Plutus.ScriptHash
deriving via
  (DerivePConstantViaBuiltin Plutus.ScriptHash PScriptHash PByteString)
  instance
    PConstantDecl Plutus.ScriptHash

instance PTryFrom PData (PAsData PScriptHash) where
  type PTryFromExcess PData (PAsData PScriptHash) = Flip Term PScriptHash
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    tcont $ \f ->
      pif (plengthBS # unwrapped #== 28) (f ()) (ptraceError "ptryFrom(PScriptHash): must be 28 bytes long")
    pure (punsafeCoerce opq, pcon . PScriptHash $ unwrapped)

newtype Flip f a b = Flip (f b a) deriving stock (Generic)
