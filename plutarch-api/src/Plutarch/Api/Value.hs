{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Value-related functionality. In order to keep the interface efficient and
 safe at the same time, there is a type-level distinction between 'PValue's
 that are guaranteed to be properly normalized and those that provide no
 such guarantee.

 Also for efficiency reasons, the Ada-specific functions assume that there
 can be only one token name for the Ada currency symbol, and they don't check
 whether it matches 'Plutus.adaToken'.
-}
module Plutarch.Api.Value (
  -- * Types
  PValue (PValue),
  PCurrencySymbol (PCurrencySymbol),
  PTokenName (PTokenName),
  AmountGuarantees (NoGuarantees, NonZero, Positive),

  -- * Functions

  -- ** Transformation
  passertPositive,
  pforgetPositive,
  pnormalize,

  -- ** Partial ordering
  pcheckBinRel,

  -- ** Combination
  punionResolvingCollisionsWith,
) where

import Plutarch.Api.AssocMap qualified as AssocMap
import Plutarch.Api.Utils (Mret)
import Plutarch.Lift (
  DerivePConstantViaBuiltin (DerivePConstantViaBuiltin),
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Prelude
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)
import PlutusLedgerApi.V2 qualified as Plutus
import PlutusTx.Prelude qualified as PlutusTx

-- | @since 2.0.0
newtype PTokenName (s :: S) = PTokenName (Term s PByteString)
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @wsince 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PTokenName where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl PTokenName where
  type PLifted PTokenName = Plutus.TokenName

-- | @since 2.0.0
deriving via
  (DerivePConstantViaBuiltin Plutus.TokenName PTokenName PByteString)
  instance
    PConstantDecl Plutus.TokenName

-- | @since 2.0.0
instance PTryFrom PData (PAsData PTokenName) where
  type PTryFromExcess PData (PAsData PTokenName) = Mret PTokenName
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    tcont $ \f ->
      pif (plengthBS # unwrapped #<= 32) (f ()) (ptraceError "ptryFrom(TokenName): must be at most 32 Bytes long")
    pure (punsafeCoerce opq, pcon . PTokenName $ unwrapped)

-- | @since 2.0.0
newtype PCurrencySymbol (s :: S) = PCurrencySymbol (Term s PByteString)
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PCurrencySymbol where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PTryFrom PData (PAsData PCurrencySymbol) where
  type PTryFromExcess PData (PAsData PCurrencySymbol) = Mret PCurrencySymbol
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    len <- tcont . plet $ plengthBS # unwrapped
    tcont $ \f ->
      pif (len #== 0 #|| len #== 28) (f ()) (ptraceError "ptryFrom(CurrencySymbol): must be 28 bytes long or empty")
    pure (punsafeCoerce opq, pcon . PCurrencySymbol $ unwrapped)

-- | @since 2.0.0
instance PUnsafeLiftDecl PCurrencySymbol where
  type PLifted PCurrencySymbol = Plutus.CurrencySymbol

-- | @since 2.0.0
deriving via
  (DerivePConstantViaBuiltin Plutus.CurrencySymbol PCurrencySymbol PByteString)
  instance
    PConstantDecl Plutus.CurrencySymbol

-- | @since 2.0.0
data AmountGuarantees = NoGuarantees | NonZero | Positive

-- | @since 2.0.0
newtype PValue (keys :: AssocMap.KeyGuarantees) (amounts :: AmountGuarantees) (s :: S)
  = PValue (Term s (AssocMap.PMap keys PCurrencySymbol (AssocMap.PMap keys PTokenName PInteger)))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PShow
    )

type role PValue nominal nominal nominal

-- | @since 2.0.0
instance DerivePlutusType (PValue keys amounts) where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl (PValue 'AssocMap.Unsorted 'NonZero) where
  type PLifted (PValue 'AssocMap.Unsorted 'NonZero) = Plutus.Value

-- | @since 2.0.0
deriving via
  ( DerivePConstantViaNewtype
      Plutus.Value
      (PValue 'AssocMap.Unsorted 'NonZero)
      (AssocMap.PMap 'AssocMap.Unsorted PCurrencySymbol (AssocMap.PMap 'AssocMap.Unsorted PTokenName PInteger))
  )
  instance
    PConstantDecl Plutus.Value

-- | @since 2.0.0
instance PEq (PValue 'AssocMap.Sorted 'Positive) where
  a #== b = pto a #== pto b

-- | @since 2.0.0
instance PEq (PValue 'AssocMap.Sorted 'NonZero) where
  a #== b = pto a #== pto b

{- | Partial ordering implementation for sorted 'PValue' with 'Positive' amounts.

Use 'pcheckBinRel' if 'AmountGuarantees' is 'NoGuarantees'.

@since 2.0.0
-}
instance PPartialOrd (PValue 'AssocMap.Sorted 'Positive) where
  (#<) ::
    forall (s :: S).
    Term s (PValue 'AssocMap.Sorted 'Positive) ->
    Term s (PValue 'AssocMap.Sorted 'Positive) ->
    Term s PBool
  a #< b = a' #< pforgetPositive b
    where
      a' :: Term s (PValue 'AssocMap.Sorted 'NonZero)
      a' = pforgetPositive a
  (#<=) ::
    forall (s :: S).
    Term s (PValue 'AssocMap.Sorted 'Positive) ->
    Term s (PValue 'AssocMap.Sorted 'Positive) ->
    Term s PBool
  a #<= b = a' #<= pforgetPositive b
    where
      a' :: Term s (PValue 'AssocMap.Sorted 'NonZero)
      a' = pforgetPositive a

{- | Partial ordering implementation for sorted 'PValue' with 'NonZero' amounts.

Use 'pcheckBinRel' if 'AmountGuarantees' is 'NoGuarantees'.

@since 2.0.0
-}
instance PPartialOrd (PValue 'AssocMap.Sorted 'NonZero) where
  a #< b = f # a # b
    where
      f ::
        forall (s :: S).
        Term
          s
          ( PValue 'AssocMap.Sorted 'NonZero
              :--> PValue 'AssocMap.Sorted 'NonZero
              :--> PBool
          )
      f = phoistAcyclic $ pcheckBinRel #$ phoistAcyclic $ plam (#<)
  a #<= b = f # a # b
    where
      f ::
        forall (s :: S).
        Term
          s
          ( PValue 'AssocMap.Sorted 'NonZero
              :--> PValue 'AssocMap.Sorted 'NonZero
              :--> PBool
          )
      f = phoistAcyclic $ pcheckBinRel #$ phoistAcyclic $ plam (#<=)

-- | @since 2.0.0
instance PEq (PValue 'AssocMap.Sorted 'NoGuarantees) where
  a #== b =
    AssocMap.pall
      # (AssocMap.pall # plam (#== 0))
      -- While '(-)' is not commutative, we don't need that property here.
      -- TODO benchmark with '(==)'
      # pto (punionResolvingCollisionsWith AssocMap.Commutative # plam (-) # a # b)

-- | @since 2.0.0
instance Semigroup (Term s (PValue 'AssocMap.Sorted 'Positive)) where
  a <> b =
    punsafeDowncast (pto $ punionResolvingCollisionsWith AssocMap.Commutative # plam (+) # a # b)

-- | @since 2.0.0
instance PlutusTx.Semigroup (Term s (PValue 'AssocMap.Sorted 'Positive)) where
  a <> b =
    punsafeDowncast (pto $ punionResolvingCollisionsWith AssocMap.Commutative # plam (+) # a # b)

-- | @since 2.0.0
instance Semigroup (Term s (PValue 'AssocMap.Sorted 'NonZero)) where
  a <> b =
    pnormalize #$ punionResolvingCollisionsWith AssocMap.Commutative # plam (+) # a # b

-- | @since 2.0.0
instance PlutusTx.Semigroup (Term s (PValue 'AssocMap.Sorted 'NonZero)) where
  a <> b =
    pnormalize #$ punionResolvingCollisionsWith AssocMap.Commutative # plam (+) # a # b

-- | @since 2.0.0
instance Semigroup (Term s (PValue 'AssocMap.Sorted 'NoGuarantees)) where
  a <> b =
    punionResolvingCollisionsWith AssocMap.Commutative # plam (+) # a # b

-- | @since 2.0.0
instance PlutusTx.Semigroup (Term s (PValue 'AssocMap.Sorted 'NoGuarantees)) where
  a <> b =
    punionResolvingCollisionsWith AssocMap.Commutative # plam (+) # a # b

-- | @since 2.0.0
instance
  Semigroup (Term s (PValue 'AssocMap.Sorted normalization)) =>
  Monoid (Term s (PValue 'AssocMap.Sorted normalization))
  where
  mempty = pcon (PValue AssocMap.pempty)

-- | @since 2.0.0
instance
  PlutusTx.Semigroup (Term s (PValue 'AssocMap.Sorted normalization)) =>
  PlutusTx.Monoid (Term s (PValue 'AssocMap.Sorted normalization))
  where
  mempty = pcon (PValue AssocMap.pempty)

-- | @since 2.0.0
instance
  PlutusTx.Semigroup (Term s (PValue 'AssocMap.Sorted 'NoGuarantees)) =>
  PlutusTx.Group (Term s (PValue 'AssocMap.Sorted 'NoGuarantees))
  where
  inv a = pmapAmounts # plam negate # a

-- | @since 2.0.0
instance
  PlutusTx.Semigroup (Term s (PValue 'AssocMap.Sorted 'NonZero)) =>
  PlutusTx.Group (Term s (PValue 'AssocMap.Sorted 'NonZero))
  where
  inv a =
    punsafeCoerce $ PlutusTx.inv (punsafeCoerce a :: Term s (PValue 'AssocMap.Sorted 'NoGuarantees))

-- | @since 2.0.0
instance PTryFrom PData (PAsData (PValue 'AssocMap.Unsorted 'NoGuarantees))

-- | @since 2.0.0
instance PTryFrom PData (PAsData (PValue 'AssocMap.Sorted 'NoGuarantees))

-- | @since 2.0.0
instance PTryFrom PData (PAsData (PValue 'AssocMap.Sorted 'Positive)) where
  type PTryFromExcess PData (PAsData (PValue 'AssocMap.Sorted 'Positive)) = Mret (PValue 'AssocMap.Sorted 'Positive)
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData (PValue 'AssocMap.Sorted 'NoGuarantees)) opq
    unwrapped <- tcont . plet . papp passertPositive . pfromData $ opq'
    pure (punsafeCoerce opq, unwrapped)

-- | @since 2.0.0
instance PTryFrom PData (PAsData (PValue 'AssocMap.Unsorted 'Positive)) where
  type PTryFromExcess PData (PAsData (PValue 'AssocMap.Unsorted 'Positive)) = Mret (PValue 'AssocMap.Unsorted 'Positive)
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData (PValue 'AssocMap.Unsorted 'NoGuarantees)) opq
    unwrapped <- tcont . plet . papp passertPositive . pfromData $ opq'
    pure (punsafeCoerce opq, unwrapped)

-- | @since 2.0.0
instance PTryFrom PData (PAsData (PValue 'AssocMap.Sorted 'NonZero)) where
  type PTryFromExcess PData (PAsData (PValue 'AssocMap.Sorted 'NonZero)) = Mret (PValue 'AssocMap.Sorted 'NonZero)
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData (PValue 'AssocMap.Sorted 'NoGuarantees)) opq
    unwrapped <- tcont . plet . papp passertNonZero . pfromData $ opq'
    pure (punsafeCoerce opq, unwrapped)

{- | \'Forget\' that a 'Value' has an only-positive guarantee.

@since 2.0.0
-}
pforgetPositive ::
  forall (a :: AmountGuarantees) (k :: AssocMap.KeyGuarantees) (s :: S).
  Term s (PValue k 'Positive) ->
  Term s (PValue k a)
pforgetPositive = punsafeCoerce

{- | Given a description of a relation on amounts, check whether that relation
holds over sorted 'PValue's.

= Important note

This is intended for use with boolean comparison functions, which must define
at least a partial order (total orders and equivalences are acceptable as
well). Use of this with anything else is not guaranteed to give anything
resembling a sensible answer. Use with extreme care.

@since 2.0.0
-}
pcheckBinRel ::
  forall (any0 :: AmountGuarantees) (any1 :: AmountGuarantees) (s :: S).
  Term
    s
    ( (PInteger :--> PInteger :--> PBool)
        :--> PValue 'AssocMap.Sorted any0
        :--> PValue 'AssocMap.Sorted any1
        :--> PBool
    )
pcheckBinRel = phoistAcyclic $
  plam $ \f ->
    punsafeCoerce @_ @_ @(PValue AssocMap.Sorted any0 :--> PValue AssocMap.Sorted any1 :--> PBool) $
      AssocMap.pcheckBinRel @PCurrencySymbol # (AssocMap.pcheckBinRel @PTokenName # f # 0) # AssocMap.pempty

{- | Combine two 'PValue's applying the given function to any pair of
 quantities with the same asset class. Note that the result is _not_
 'normalize'd and may contain zero quantities.

 @since 2.0.0
-}
punionResolvingCollisionsWith ::
  forall (any0 :: AmountGuarantees) (any1 :: AmountGuarantees) (s :: S).
  AssocMap.Commutativity ->
  Term
    s
    ( (PInteger :--> PInteger :--> PInteger)
        :--> PValue 'AssocMap.Sorted any0
        :--> PValue 'AssocMap.Sorted any1
        :--> PValue 'AssocMap.Sorted 'NoGuarantees
    )
punionResolvingCollisionsWith commutativity = phoistAcyclic $
  plam $ \combine x y ->
    pcon . PValue $
      AssocMap.punionResolvingCollisionsWith commutativity
        # plam (\x' y' -> AssocMap.punionResolvingCollisionsWith commutativity # combine # x' # y')
        # pto x
        # pto y

{- | Normalize the argument to contain no zero quantity nor empty token map.

@since 2.0.0
-}
pnormalize ::
  forall (any :: AmountGuarantees) (s :: S).
  Term s (PValue 'AssocMap.Sorted any :--> PValue 'AssocMap.Sorted 'NonZero)
pnormalize = phoistAcyclic $
  plam $ \value ->
    pcon . PValue $
      AssocMap.pmapMaybe # plam normalizeTokenMap # pto value
  where
    normalizeTokenMap ::
      forall (s' :: S) (k :: S -> Type) (any1 :: AssocMap.KeyGuarantees).
      Term s' (AssocMap.PMap any1 k PInteger) ->
      Term s' (PMaybe (AssocMap.PMap any1 k PInteger))
    normalizeTokenMap tokenMap =
      plet (AssocMap.pmapMaybeData # plam nonZero # tokenMap) $ \normalMap ->
        pif
          (AssocMap.pnull # normalMap)
          (pcon PNothing)
          (pcon $ PJust normalMap)
    nonZero ::
      forall (s' :: S).
      Term s' (PAsData PInteger) ->
      Term s' (PMaybe (PAsData PInteger))
    nonZero intData =
      pif (intData #== zeroData) (pcon PNothing) (pcon $ PJust intData)

{- | Given a 'PValue', either construct another 'PValue' with the same contents
and a proof that all amounts in it are positive, or error.

@since 2.0.0
-}
passertPositive ::
  forall (kg :: AssocMap.KeyGuarantees) (ag :: AmountGuarantees) (s :: S).
  Term s (PValue kg ag :--> PValue kg 'Positive)
passertPositive = phoistAcyclic $
  plam $ \value ->
    pif
      ( AssocMap.pall
          # plam (\submap -> AssocMap.pall # plam (0 #<) # submap)
          # pto value
      )
      (punsafeDowncast $ pto value)
      (ptraceError "Negative amount in Value")

-- Helpers

zeroData :: forall (s :: S). Term s (PAsData PInteger)
zeroData = pdata 0

-- Applies a function to every amount in the map.
pmapAmounts ::
  forall (k :: AssocMap.KeyGuarantees) (a :: AmountGuarantees) (s :: S).
  Term s ((PInteger :--> PInteger) :--> PValue k a :--> PValue k 'NoGuarantees)
pmapAmounts = phoistAcyclic $
  plam $
    \f v -> pcon $ PValue $ AssocMap.pmap # plam (AssocMap.pmap # f #) # pto v

passertNonZero ::
  forall (kg :: AssocMap.KeyGuarantees) (ag :: AmountGuarantees).
  ( forall (s :: S). Term s (PValue kg ag :--> PValue kg 'NonZero)
  )
passertNonZero = plam $ \val ->
  pif (outer #$ pto . pto $ val) (punsafeCoerce val) (ptraceError "Zero amount in Value")
  where
    outer ::
      forall (s' :: S) (k :: AssocMap.KeyGuarantees).
      Term s' (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (AssocMap.PMap k PTokenName PInteger))) :--> PBool)
    outer = pfix #$ plam $ \self m ->
      pmatch m $ \case
        PCons x xs -> inner # (pto . pfromData $ psndBuiltin # x) #&& self # xs
        PNil -> pcon PTrue
    inner :: ClosedTerm (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBool)
    inner = pfix #$ plam $ \self m ->
      pmatch m $ \case
        PCons x xs -> pnot # (psndBuiltin # x #== pconstantData 0) #&& self # xs
        PNil -> pcon PTrue
