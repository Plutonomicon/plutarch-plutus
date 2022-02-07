{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Scripts (
  PDatum (PDatum),
  PDatumHash (PDatumHash),
  PRedeemer (PRedeemer),
  PRedeemerHash (PRedeemerHash),
  PStakeValidatorHash (PStakeValidatorHash),
  PValidatorHash (PValidatorHash),
) where

import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Scripts as PlutusScript
import qualified PlutusTx.Builtins.Internal as PT

import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Prelude

newtype PDatum (s :: S) = PDatum (Term s PData)
  deriving (PlutusType, PIsData, PEq) via (DerivePNewtype PDatum PData)

instance PUnsafeLiftDecl PDatum where type PLifted PDatum = Plutus.Datum
deriving via (DerivePConstantViaNewtype Plutus.Datum PDatum PData) instance (PConstant Plutus.Datum)

newtype PRedeemer (s :: S) = PRedeemer (Term s PData)
  deriving (PlutusType, PIsData, PEq) via (DerivePNewtype PRedeemer PData)

instance PUnsafeLiftDecl PRedeemer where type PLifted PRedeemer = Plutus.Redeemer
deriving via (DerivePConstantViaNewtype Plutus.Redeemer PRedeemer PData) instance (PConstant Plutus.Redeemer)

newtype PDatumHash (s :: S) = PDatumHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PDatumHash PByteString)

instance PUnsafeLiftDecl PDatumHash where type PLifted PDatumHash = Plutus.DatumHash
deriving via (DerivePConstantViaNewtype Plutus.DatumHash PDatumHash PByteString) instance (PConstant Plutus.DatumHash)

newtype PStakeValidatorHash (s :: S) = PStakeValidatorHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PStakeValidatorHash PByteString)

instance PUnsafeLiftDecl PStakeValidatorHash where type PLifted PStakeValidatorHash = Plutus.StakeValidatorHash
deriving via
  (DerivePConstantViaNewtype Plutus.StakeValidatorHash PStakeValidatorHash PByteString)
  instance
    (PConstant Plutus.StakeValidatorHash)

newtype PRedeemerHash (s :: S) = PRedeemerHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PRedeemerHash PByteString)

instance PUnsafeLiftDecl PRedeemerHash where type PLifted PRedeemerHash = Plutus.RedeemerHash
deriving via
  (DerivePConstantViaNewtype Plutus.RedeemerHash PRedeemerHash PByteString)
  instance
    (PConstant Plutus.RedeemerHash)

newtype PValidatorHash (s :: S) = PValidatorHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PValidatorHash PByteString)

instance PUnsafeLiftDecl PValidatorHash where type PLifted PValidatorHash = Plutus.ValidatorHash
deriving via
  (DerivePConstantViaNewtype Plutus.ValidatorHash PValidatorHash PByteString)
  instance
    (PConstant Plutus.ValidatorHash)

newtype PScriptHash (s :: S) = PScriptHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PScriptHash PByteString)

instance PUnsafeLiftDecl PScriptHash where type PLifted PScriptHash = PlutusScript.ScriptHash
deriving via
  (DerivePConstantViaNewtype Plutus.ValidatorHash PScriptHash PByteString)
  instance
    (PConstant PlutusScript.ScriptHash)

newtype PMintingPolicyHash (s :: S) = PMintingPolicyHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PMintingPolicyHash PByteString)

instance PUnsafeLiftDecl PMintingPolicyHash where type PLifted PMintingPolicyHash = PlutusScript.MintingPolicyHash
deriving via
  (DerivePConstantViaNewtype Plutus.ValidatorHash PMintingPolicyHash PByteString)
  instance
    (PConstant PlutusScript.MintingPolicyHash)
