{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Ada (PAda (PLovelace)) where

import Plutarch.Lift (DerivePConstantViaNewtype (DerivePConstantViaNewtype), PLifted, PUnsafeLiftDecl)
import Plutarch.Prelude
import qualified Plutus.V1.Ledger.Ada as PlutusAda

newtype PAda (s :: S) = PLovelace (Term s PInteger)
  deriving (PlutusType, PIsData, PEq) via (DerivePNewtype PAda PInteger)

instance PUnsafeLiftDecl PAda where type PLifted PAda = PlutusAda.Ada
deriving via (DerivePConstantViaNewtype PlutusAda.Ada PAda PInteger) instance (PConstant PlutusAda.Ada)
