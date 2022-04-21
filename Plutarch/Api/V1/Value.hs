{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Value (
  PValue (PValue),
  PCurrencySymbol (PCurrencySymbol),
  PTokenName (PTokenName),
  isZero,
  singleton,
  unionWith,
  unionWithData,
  valueOf,
  normalize,
) where

import qualified Plutus.V1.Ledger.Api as Plutus

import Plutarch.Api.V1.AssocMap (PMap)
import qualified Plutarch.Api.V1.AssocMap as AssocMap
import Plutarch.Lift (
  DerivePConstantViaBuiltin (DerivePConstantViaBuiltin),
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Unsafe (punsafeFrom)

import Plutarch.Prelude

newtype PTokenName (s :: S) = PTokenName (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PTokenName PByteString)

instance PUnsafeLiftDecl PTokenName where type PLifted PTokenName = Plutus.TokenName
deriving via
  (DerivePConstantViaBuiltin Plutus.TokenName PTokenName PByteString)
  instance
    PConstantDecl Plutus.TokenName

newtype PCurrencySymbol (s :: S) = PCurrencySymbol (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PCurrencySymbol PByteString)

instance PUnsafeLiftDecl PCurrencySymbol where type PLifted PCurrencySymbol = Plutus.CurrencySymbol
deriving via
  (DerivePConstantViaBuiltin Plutus.CurrencySymbol PCurrencySymbol PByteString)
  instance
    PConstantDecl Plutus.CurrencySymbol

newtype PValue (s :: S) = PValue (Term s (PMap PCurrencySymbol (PMap PTokenName PInteger)))
  deriving
    (PlutusType, PIsData)
    via (DerivePNewtype PValue (PMap PCurrencySymbol (PMap PTokenName PInteger)))

instance PUnsafeLiftDecl PValue where type PLifted PValue = Plutus.Value
deriving via
  (DerivePConstantViaNewtype Plutus.Value PValue (PMap PCurrencySymbol (PMap PTokenName PInteger)))
  instance
    PConstantDecl Plutus.Value

-- | Works correctly only on 'normalize'd values!
instance PEq PValue where
  a #== b = pto a #== pto b

-- | Construct a singleton 'PValue' containing only the given quantity of the given currency.
singleton :: Term (s :: S) (PCurrencySymbol :--> PTokenName :--> PInteger :--> PValue)
singleton = phoistAcyclic $
  plam $ \symbol token amount ->
    punsafeFrom (AssocMap.singleton # symbol #$ AssocMap.singleton # token # amount)

-- | Get the quantity of the given currency in the 'PValue'.
valueOf :: Term (s :: S) (PValue :--> PCurrencySymbol :--> PTokenName :--> PInteger)
valueOf = phoistAcyclic $
  plam $ \value symbol token ->
    AssocMap.foldAt
      # symbol
      # 0
      # plam (\map -> AssocMap.foldAt # token # 0 # plam pfromData # pfromData map)
      # pto value

-- | Check if the value is zero.
isZero :: Term (s :: S) (PValue :--> PBool)
isZero = phoistAcyclic $
  plam $ \value -> AssocMap.all # (AssocMap.all # plam (#== 0)) # pto value

{- | Combine two 'PValue's applying the given function to any pair of
 quantities with the same asset class. Note that the result is _not_
 'normalize'd and may contain zero quantities.
-}
unionWith :: Term (s :: S) ((PInteger :--> PInteger :--> PInteger) :--> PValue :--> PValue :--> PValue)
unionWith = phoistAcyclic $
  plam $ \combine x y ->
    pcon . PValue $
      AssocMap.unionWith
        # (plam $ \x y -> AssocMap.unionWith # combine # x # y)
        # pto x
        # pto y

{- | Combine two 'PValue's applying the given function to any pair of
 data-encoded quantities with the same asset class. Note that the result is
 _not_ 'normalize'd and may contain zero quantities.
-}
unionWithData ::
  Term
    (s :: S)
    ( (PAsData PInteger :--> PAsData PInteger :--> PAsData PInteger)
        :--> PValue
        :--> PValue
        :--> PValue
    )
unionWithData = phoistAcyclic $
  plam $ \combine x y ->
    pcon . PValue $
      AssocMap.unionWith
        # (plam $ \x y -> AssocMap.unionWithData # combine # x # y)
        # pto x
        # pto y

-- | Normalize the argument to contain no zero quantity nor empty token map.
normalize :: Term s (PValue :--> PValue)
normalize = phoistAcyclic $
  plam $ \value ->
    pcon . PValue $
      AssocMap.mapMaybe # plam normalizeTokenMap # pto value
  where
    normalizeTokenMap tokenMap =
      plet (AssocMap.mapMaybeData # plam nonZero # tokenMap) $ \normalMap ->
        pif
          (AssocMap.null # normalMap)
          (pcon PNothing)
          (pcon $ PJust normalMap)
    nonZero intData =
      pif (intData #== zeroData) (pcon PNothing) (pcon $ PJust intData)

zeroData :: ClosedTerm (PAsData PInteger)
zeroData = pdata 0
