{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Scripts (
  -- * Plutus API Types
  PDatum (PDatum),
  PDatumHash (PDatumHash),
  PRedeemer (PRedeemer),
  PRedeemerHash (PRedeemerHash),
  PStakeValidatorHash (PStakeValidatorHash),
  PValidatorHash (PValidatorHash),
  PScriptHash (PScriptHash),
) where

import qualified PlutusLedgerApi.V1 as Plutus
import qualified PlutusLedgerApi.V1.Scripts as Plutus

import Plutarch.Builtin (Flip)
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
  deriving anyclass (PlutusType, PIsData, PEq)
instance DerivePlutusType PDatum where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PDatum where type PLifted PDatum = Plutus.Datum
deriving via (DerivePConstantViaBuiltin Plutus.Datum PDatum PData) instance PConstantDecl Plutus.Datum

newtype PRedeemer (s :: S) = PRedeemer (Term s PData)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)
instance DerivePlutusType PRedeemer where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PRedeemer where type PLifted PRedeemer = Plutus.Redeemer
deriving via (DerivePConstantViaBuiltin Plutus.Redeemer PRedeemer PData) instance PConstantDecl Plutus.Redeemer

newtype PDatumHash (s :: S) = PDatumHash (Term s PByteString)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, POrd)
instance DerivePlutusType PDatumHash where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PDatumHash where type PLifted PDatumHash = Plutus.DatumHash
deriving via (DerivePConstantViaBuiltin Plutus.DatumHash PDatumHash PByteString) instance PConstantDecl Plutus.DatumHash

newtype PStakeValidatorHash (s :: S) = PStakeValidatorHash (Term s PByteString)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, POrd)
instance DerivePlutusType PStakeValidatorHash where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PStakeValidatorHash where type PLifted PStakeValidatorHash = Plutus.StakeValidatorHash
deriving via
  (DerivePConstantViaBuiltin Plutus.StakeValidatorHash PStakeValidatorHash PByteString)
  instance
    PConstantDecl Plutus.StakeValidatorHash

newtype PRedeemerHash (s :: S) = PRedeemerHash (Term s PByteString)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, POrd)
instance DerivePlutusType PRedeemerHash where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PRedeemerHash where type PLifted PRedeemerHash = Plutus.RedeemerHash
deriving via
  (DerivePConstantViaBuiltin Plutus.RedeemerHash PRedeemerHash PByteString)
  instance
    PConstantDecl Plutus.RedeemerHash

newtype PValidatorHash (s :: S) = PValidatorHash (Term s PByteString)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, POrd)
instance DerivePlutusType PValidatorHash where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PValidatorHash where type PLifted PValidatorHash = Plutus.ValidatorHash
deriving via
  (DerivePConstantViaBuiltin Plutus.ValidatorHash PValidatorHash PByteString)
  instance
    PConstantDecl Plutus.ValidatorHash

instance PTryFrom PData (PAsData PValidatorHash) where
  type PTryFromExcess PData (PAsData PValidatorHash) = Flip Term PValidatorHash
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    tcont $ \f -> 
      pif (plengthBS # unwrapped #== 28) (f ()) (ptraceError "a ValidatorHash must be 28 bytes long")
    pure (punsafeCoerce opq, pcon . PValidatorHash $ unwrapped)

newtype PMintingPolicyHash (s :: S) = PMintingPolicyHash (Term s PByteString)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, POrd)
instance DerivePlutusType PMintingPolicyHash where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PMintingPolicyHash where type PLifted PMintingPolicyHash = Plutus.MintingPolicyHash
deriving via
  (DerivePConstantViaBuiltin Plutus.MintingPolicyHash PMintingPolicyHash PByteString)
  instance
    PConstantDecl Plutus.MintingPolicyHash

newtype PScriptHash (s :: S) = PScriptHash (Term s PByteString)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, POrd)
instance DerivePlutusType PScriptHash where type DPTStrat _ = PlutusTypeNewtype

instance PUnsafeLiftDecl PScriptHash where type PLifted PScriptHash = Plutus.ScriptHash
deriving via
  (DerivePConstantViaBuiltin Plutus.ScriptHash PScriptHash PByteString)
  instance
    PConstantDecl Plutus.ScriptHash
