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
) where

import qualified Plutus.V1.Ledger.Api as Plutus

import Plutarch.Lift (
  DerivePConstantViaBuiltin (DerivePConstantViaBuiltin),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Prelude

newtype PDatum (s :: S) = PDatum (Term s PData)
  deriving (PlutusType, PIsData, PEq) via (DerivePNewtype PDatum PData)

instance PUnsafeLiftDecl PDatum where type PLifted PDatum = Plutus.Datum
deriving via (DerivePConstantViaBuiltin Plutus.Datum PDatum PData) instance PConstantDecl Plutus.Datum

newtype PRedeemer (s :: S) = PRedeemer (Term s PData)
  deriving (PlutusType, PIsData, PEq) via (DerivePNewtype PRedeemer PData)

instance PUnsafeLiftDecl PRedeemer where type PLifted PRedeemer = Plutus.Redeemer
deriving via (DerivePConstantViaBuiltin Plutus.Redeemer PRedeemer PData) instance PConstantDecl Plutus.Redeemer

newtype PDatumHash (s :: S) = PDatumHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PDatumHash PByteString)

instance PUnsafeLiftDecl PDatumHash where type PLifted PDatumHash = Plutus.DatumHash
deriving via (DerivePConstantViaBuiltin Plutus.DatumHash PDatumHash PByteString) instance PConstantDecl Plutus.DatumHash

newtype PStakeValidatorHash (s :: S) = PStakeValidatorHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PStakeValidatorHash PByteString)

instance PUnsafeLiftDecl PStakeValidatorHash where type PLifted PStakeValidatorHash = Plutus.StakeValidatorHash
deriving via
  (DerivePConstantViaBuiltin Plutus.StakeValidatorHash PStakeValidatorHash PByteString)
  instance
    PConstantDecl Plutus.StakeValidatorHash

newtype PRedeemerHash (s :: S) = PRedeemerHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PRedeemerHash PByteString)

instance PUnsafeLiftDecl PRedeemerHash where type PLifted PRedeemerHash = Plutus.RedeemerHash
deriving via
  (DerivePConstantViaBuiltin Plutus.RedeemerHash PRedeemerHash PByteString)
  instance
    PConstantDecl Plutus.RedeemerHash

newtype PValidatorHash (s :: S) = PValidatorHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PValidatorHash PByteString)

instance PUnsafeLiftDecl PValidatorHash where type PLifted PValidatorHash = Plutus.ValidatorHash
deriving via
  (DerivePConstantViaBuiltin Plutus.ValidatorHash PValidatorHash PByteString)
  instance
    PConstantDecl Plutus.ValidatorHash
