{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Value (
  PValue (PValue),
  PCurrencySymbol (PCurrencySymbol),
  PTokenName (PTokenName),
  PAssetClass (PAssetClass),
) where

import qualified Plutus.V1.Ledger.Value as PlutusValue
import qualified PlutusTx.Builtins.Internal as PT

import Plutarch.Api.V1.AssocMap (PMap)
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantRepr,
  PConstanted,
  PLifted,
  PUnsafeLiftDecl,
  pconstantFromRepr,
  pconstantToRepr,
 )
import Plutarch.Prelude
import qualified PlutusTx

newtype PTokenName (s :: S) = PTokenName (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PTokenName PByteString)

instance PUnsafeLiftDecl PTokenName where type PLifted PTokenName = PlutusValue.TokenName
deriving via
  (DerivePConstantViaNewtype PlutusValue.TokenName PTokenName PByteString)
  instance
    (PConstant PlutusValue.TokenName)

newtype PCurrencySymbol (s :: S) = PCurrencySymbol (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PCurrencySymbol PByteString)

instance PUnsafeLiftDecl PCurrencySymbol where type PLifted PCurrencySymbol = PlutusValue.CurrencySymbol
deriving via
  (DerivePConstantViaNewtype PlutusValue.CurrencySymbol PCurrencySymbol PByteString)
  instance
    (PConstant PlutusValue.CurrencySymbol)

newtype PValue (s :: S) = PValue (Term s (PMap PCurrencySymbol (PMap PTokenName PInteger)))
  deriving
    (PlutusType, PIsData)
    via (DerivePNewtype PValue (PMap PCurrencySymbol (PMap PTokenName PInteger)))

instance PUnsafeLiftDecl PValue where type PLifted PValue = PlutusValue.Value
deriving via
  (DerivePConstantViaNewtype PlutusValue.Value PValue (PMap PCurrencySymbol (PMap PTokenName PInteger)))
  instance
    (PConstant PlutusValue.Value)

newtype PAssetClass (s :: S) = PAssetClass (Term s (PBuiltinPair (PAsData PCurrencySymbol) (PAsData PTokenName)))
  deriving
    (PlutusType, PIsData)
    via (DerivePNewtype PAssetClass (PBuiltinPair (PAsData PCurrencySymbol) (PAsData PTokenName)))

instance PUnsafeLiftDecl PAssetClass where type PLifted PAssetClass = PlutusValue.AssetClass

instance PConstant PlutusValue.AssetClass where
  type PConstantRepr PlutusValue.AssetClass = (PlutusTx.Data, PlutusTx.Data)
  type PConstanted PlutusValue.AssetClass = PAssetClass
  pconstantToRepr (PlutusValue.AssetClass (x, y)) = (PlutusTx.toData x, PlutusTx.toData y)
  pconstantFromRepr (x, y) = fmap PlutusValue.AssetClass $ (,) <$> PlutusTx.fromData x <*> PlutusTx.fromData y
