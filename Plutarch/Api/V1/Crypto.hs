{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Crypto (
  PPubKeyHash (PPubKeyHash),
  PPubKey (PPubKey),
  PSignature (PSignature),
) where

import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Crypto as PlutusCrypto
import qualified PlutusTx.Builtins.Internal as PT

import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Prelude

newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PPubKeyHash PByteString)

instance PUnsafeLiftDecl PPubKeyHash where type PLifted PPubKeyHash = Plutus.PubKeyHash
deriving via
  (DerivePConstantViaNewtype Plutus.PubKeyHash PPubKeyHash PByteString)
  instance
    (PConstant Plutus.PubKeyHash)

newtype PPubKey (s :: S) = PPubKey (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PPubKey PByteString)

instance PUnsafeLiftDecl PPubKey where type PLifted PPubKey = PlutusCrypto.PubKey
deriving via
  (DerivePConstantViaNewtype PlutusCrypto.PubKey PPubKey PByteString)
  instance
    (PConstant PlutusCrypto.PubKey)

newtype PSignature (s :: S) = PSignature (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PSignature PByteString)

instance PUnsafeLiftDecl PSignature where type PLifted PSignature = PlutusCrypto.Signature
deriving via
  (DerivePConstantViaNewtype PlutusCrypto.Signature PSignature PByteString)
  instance
    (PConstant PlutusCrypto.Signature)
