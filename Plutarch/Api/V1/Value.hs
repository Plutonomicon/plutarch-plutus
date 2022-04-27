{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Value (
  PValue (PValue),
  PCurrencySymbol (PCurrencySymbol),
  PTokenName (PTokenName),
  ValueState (Sorted, Unverified),
  ValueNormalization (Normalized, NotNormalized),
  assertSorted,
  isZero,
  singleton,
  singletonData,
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

data ValueState = Sorted ValueNormalization | Unverified
data ValueNormalization = Normalized | NotNormalized

newtype PValue (state :: ValueState) (s :: S)
  = PValue (Term s (PMap PCurrencySymbol (PMap PTokenName PInteger)))
  deriving
    (PlutusType, PIsData)
    via (DerivePNewtype (PValue state) (PMap PCurrencySymbol (PMap PTokenName PInteger)))
type role PValue nominal phantom

instance PUnsafeLiftDecl (PValue ( 'Sorted 'Normalized)) where
  type PLifted (PValue ( 'Sorted 'Normalized)) = Plutus.Value
deriving via
  ( DerivePConstantViaNewtype
      Plutus.Value
      (PValue ( 'Sorted 'Normalized))
      (PMap PCurrencySymbol (PMap PTokenName PInteger))
  )
  instance
    PConstantDecl Plutus.Value

instance PEq (PValue ( 'Sorted 'Normalized)) where
  a #== b = pto a #== pto b

instance PEq (PValue ( 'Sorted 'NotNormalized)) where
  a #== b = AssocMap.all # (AssocMap.all # plam (#== 0)) # pto (unionWith # plam (-) # a # b)

instance Semigroup (Term s (PValue ( 'Sorted normalization))) where
  a <> b = pcon (PValue $ pto $ unionWith # plam (+) # a # b)

instance Monoid (Term s (PValue ( 'Sorted normalization))) where
  mempty = pcon (PValue AssocMap.empty)

-- | Construct a singleton 'PValue' containing only the given quantity of the given currency.
singleton ::
  Term
    (s :: S)
    (PCurrencySymbol :--> PTokenName :--> PInteger :--> PValue ( 'Sorted 'Normalized))
singleton = phoistAcyclic $
  plam $ \symbol token amount ->
    punsafeFrom (AssocMap.singleton # symbol #$ AssocMap.singleton # token # amount)

{- | Construct a singleton 'PValue' containing only the given quantity of the
 given currency, taking data-encoded parameters.
-}
singletonData ::
  Term
    (s :: S)
    ( PAsData PCurrencySymbol :--> PAsData PTokenName :--> PAsData PInteger
        :--> PValue ( 'Sorted 'Normalized)
    )
singletonData = phoistAcyclic $
  plam $ \symbol token amount ->
    punsafeFrom
      ( AssocMap.singletonData # symbol
          #$ pdata
          $ AssocMap.singletonData # token # amount
      )

-- | Get the quantity of the given currency in the 'PValue'.
valueOf :: Term (s :: S) (PValue _ :--> PCurrencySymbol :--> PTokenName :--> PInteger)
valueOf = phoistAcyclic $
  plam $ \value symbol token ->
    AssocMap.foldAt
      # symbol
      # 0
      # plam (\map -> AssocMap.foldAt # token # 0 # plam pfromData # pfromData map)
      # pto value

-- | Check if the value is zero.
isZero :: Term (s :: S) (PValue ( 'Sorted 'Normalized) :--> PBool)
isZero = plam (\v -> AssocMap.null # pto v)

{- | Combine two 'PValue's applying the given function to any pair of
 quantities with the same asset class. Note that the result is _not_
 'normalize'd and may contain zero quantities.
-}
unionWith ::
  Term
    (s :: S)
    ( (PInteger :--> PInteger :--> PInteger) :--> PValue ( 'Sorted _) :--> PValue ( 'Sorted _)
        :--> PValue ( 'Sorted 'NotNormalized)
    )
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
        :--> PValue ( 'Sorted _)
        :--> PValue ( 'Sorted _)
        :--> PValue ( 'Sorted 'NotNormalized)
    )
unionWithData = phoistAcyclic $
  plam $ \combine x y ->
    pcon . PValue $
      AssocMap.unionWith
        # (plam $ \x y -> AssocMap.unionWithData # combine # x # y)
        # pto x
        # pto y

-- | Normalize the argument to contain no zero quantity nor empty token map.
normalize :: Term s (PValue ( 'Sorted _) :--> PValue ( 'Sorted 'Normalized))
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

-- | Assert the value is properly sorted and normalized.
assertSorted :: Term s (PValue _ :--> PValue ( 'Sorted 'Normalized))
assertSorted = phoistAcyclic $
  plam $ \value ->
    pif
      ( AssocMap.any
          # ( plam $
                \submap ->
                  AssocMap.null # (AssocMap.assertSorted # submap)
                    #|| AssocMap.any # plam (#== 0) # submap
            )
          # pto value
      )
      (ptraceError "Abnormal Value")
      (pcon $ PValue $ AssocMap.assertSorted # pto value)

zeroData :: ClosedTerm (PAsData PInteger)
zeroData = pdata 0
