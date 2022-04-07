{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Crypto (
  PPubKeyHash (PPubKeyHash),
) where

import qualified Plutus.V1.Ledger.Api as Plutus

import Plutarch.Lift (
  DerivePConstantViaBuiltin (DerivePConstantViaBuiltin),
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Prelude

newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PPubKeyHash PByteString)

instance PUnsafeLiftDecl PPubKeyHash where type PLifted PPubKeyHash = Plutus.PubKeyHash
deriving via
  (DerivePConstantViaBuiltin Plutus.PubKeyHash PPubKeyHash PByteString)
  instance
    (PConstant Plutus.PubKeyHash)
