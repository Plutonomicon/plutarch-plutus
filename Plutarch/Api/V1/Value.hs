{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Value (
  PValue (PValue),
  PCurrencySymbol (PCurrencySymbol),
  PTokenName (PTokenName),
  ValueState (Sorted, Unverified),
  ValueNormalization (Normalized, NotNormalized),
  passertSorted,
  pnormalize,
  pisZero,
  psingleton,
  psingletonData,
  punionWith,
  punionWithData,
  pvalueOf,
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

import Plutarch.Prelude hiding (psingleton)

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
  a #== b = AssocMap.pall # (AssocMap.pall # plam (#== 0)) # pto (punionWith # plam (-) # a # b)

instance Semigroup (Term s (PValue ( 'Sorted normalization))) where
  a <> b = pcon (PValue $ pto $ punionWith # plam (+) # a # b)

instance Monoid (Term s (PValue ( 'Sorted normalization))) where
  mempty = pcon (PValue AssocMap.pempty)

-- | Construct a singleton 'PValue' containing only the given quantity of the given currency.
psingleton ::
  Term
    (s :: S)
    (PCurrencySymbol :--> PTokenName :--> PInteger :--> PValue ( 'Sorted 'Normalized))
psingleton = phoistAcyclic $
  plam $ \symbol token amount ->
    punsafeFrom (AssocMap.psingleton # symbol #$ AssocMap.psingleton # token # amount)

{- | Construct a singleton 'PValue' containing only the given quantity of the
 given currency, taking data-encoded parameters.
-}
psingletonData ::
  Term
    (s :: S)
    ( PAsData PCurrencySymbol :--> PAsData PTokenName :--> PAsData PInteger
        :--> PValue ( 'Sorted 'Normalized)
    )
psingletonData = phoistAcyclic $
  plam $ \symbol token amount ->
    punsafeFrom
      ( AssocMap.psingletonData # symbol
          #$ pdata
          $ AssocMap.psingletonData # token # amount
      )

-- | Get the quantity of the given currency in the 'PValue'.
pvalueOf :: Term (s :: S) (PValue _ :--> PCurrencySymbol :--> PTokenName :--> PInteger)
pvalueOf = phoistAcyclic $
  plam $ \value symbol token ->
    AssocMap.pfoldAt
      # symbol
      # 0
      # plam (\map -> AssocMap.pfoldAt # token # 0 # plam pfromData # pfromData map)
      # pto value

-- | Check if the value is zero.
pisZero :: Term (s :: S) (PValue ( 'Sorted 'Normalized) :--> PBool)
pisZero = plam (\v -> AssocMap.pnull # pto v)

{- | Combine two 'PValue's applying the given function to any pair of
 quantities with the same asset class. Note that the result is _not_
 'normalize'd and may contain zero quantities.
-}
punionWith ::
  Term
    (s :: S)
    ( (PInteger :--> PInteger :--> PInteger) :--> PValue ( 'Sorted _) :--> PValue ( 'Sorted _)
        :--> PValue ( 'Sorted 'NotNormalized)
    )
punionWith = phoistAcyclic $
  plam $ \combine x y ->
    pcon . PValue $
      AssocMap.punionWith
        # (plam $ \x y -> AssocMap.punionWith # combine # x # y)
        # pto x
        # pto y

{- | Combine two 'PValue's applying the given function to any pair of
 data-encoded quantities with the same asset class. Note that the result is
 _not_ 'normalize'd and may contain zero quantities.
-}
punionWithData ::
  Term
    (s :: S)
    ( (PAsData PInteger :--> PAsData PInteger :--> PAsData PInteger)
        :--> PValue ( 'Sorted _)
        :--> PValue ( 'Sorted _)
        :--> PValue ( 'Sorted 'NotNormalized)
    )
punionWithData = phoistAcyclic $
  plam $ \combine x y ->
    pcon . PValue $
      AssocMap.punionWith
        # (plam $ \x y -> AssocMap.punionWithData # combine # x # y)
        # pto x
        # pto y

-- | Normalize the argument to contain no zero quantity nor empty token map.
pnormalize :: Term s (PValue ( 'Sorted _) :--> PValue ( 'Sorted 'Normalized))
pnormalize = phoistAcyclic $
  plam $ \value ->
    pcon . PValue $
      AssocMap.pmapMaybe # plam normalizeTokenMap # pto value
  where
    normalizeTokenMap tokenMap =
      plet (AssocMap.pmapMaybeData # plam nonZero # tokenMap) $ \normalMap ->
        pif
          (AssocMap.pnull # normalMap)
          (pcon PNothing)
          (pcon $ PJust normalMap)
    nonZero intData =
      pif (intData #== zeroData) (pcon PNothing) (pcon $ PJust intData)

-- | Assert the value is properly sorted and normalized.
passertSorted :: Term s (PValue _ :--> PValue ( 'Sorted 'Normalized))
passertSorted = phoistAcyclic $
  plam $ \value ->
    pif
      ( AssocMap.pany
          # ( plam $
                \submap ->
                  AssocMap.pnull # (AssocMap.passertSorted # submap)
                    #|| AssocMap.pany # plam (#== 0) # submap
            )
          # pto value
      )
      (ptraceError "Abnormal Value")
      (pcon $ PValue $ AssocMap.passertSorted # pto value)

zeroData :: ClosedTerm (PAsData PInteger)
zeroData = pdata 0
