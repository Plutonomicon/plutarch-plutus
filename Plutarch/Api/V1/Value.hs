{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Value (
  PValue (PValue),
  PCurrencySymbol (PCurrencySymbol),
  PTokenName (PTokenName),
) where

import qualified Plutus.V1.Ledger.Api as Plutus

import Plutarch.Api.V1.AssocMap (PMap)
import Plutarch.Lift (
  DerivePConstantViaBuiltin (DerivePConstantViaBuiltin),
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PLifted,
  PUnsafeLiftDecl,
 )

import Plutarch.TryFrom (
  HRecP (HNil),
  PTryFrom (PTryFromExcess, ptryFrom),
 )

import Plutarch.Prelude

newtype PTokenName (s :: S) = PTokenName (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PTokenName PByteString)

instance PUnsafeLiftDecl PTokenName where type PLifted PTokenName = Plutus.TokenName
deriving via
  (DerivePConstantViaBuiltin Plutus.TokenName PTokenName PByteString)
  instance
    (PConstant Plutus.TokenName)

newtype PCurrencySymbol (s :: S) = PCurrencySymbol (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PCurrencySymbol PByteString)

instance PUnsafeLiftDecl PCurrencySymbol where type PLifted PCurrencySymbol = Plutus.CurrencySymbol
deriving via
  (DerivePConstantViaBuiltin Plutus.CurrencySymbol PCurrencySymbol PByteString)
  instance
    (PConstant Plutus.CurrencySymbol)

newtype PValue (s :: S) = PValue (Term s (PMap PCurrencySymbol (PMap PTokenName PInteger)))
  deriving
    (PlutusType, PIsData)
    via (DerivePNewtype PValue (PMap PCurrencySymbol (PMap PTokenName PInteger)))

instance PUnsafeLiftDecl PValue where type PLifted PValue = Plutus.Value
deriving via
  (DerivePConstantViaNewtype Plutus.Value PValue (PMap PCurrencySymbol (PMap PTokenName PInteger)))
  instance
    (PConstant Plutus.Value)

----------------------- PTryFrom instances ----------------------------------------------

instance PTryFrom (PMap PCurrencySymbol (PMap PTokenName PInteger)) PValue where
  type PTryFromExcess (PMap PCurrencySymbol (PMap PTokenName PInteger)) PValue = HRecP '[]
  ptryFrom m = runTermCont $ do
    let predInner :: Term _ (PBuiltinPair (PAsData PTokenName) (PAsData PInteger) :--> PBool)
        predInner = plam $ \tup -> pif (0 #< (pfromData $ psndBuiltin # tup)) (pcon PTrue) perror
        predOuter :: Term _ (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap PTokenName PInteger)) :--> PBool)
        predOuter = plam $ \tup -> pall # predInner # (pto $ pfromData $ psndBuiltin # tup)
        res :: Term _ PBool
        res = pall # predOuter # pto m
    _ <- tcont $ plet res
    pure $ (pcon $ PValue m, HNil)
