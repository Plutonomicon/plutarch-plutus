{-# LANGUAGE RankNTypes #-}
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
module Plutarch.LedgerApi.Value (
  -- * Types
  PValue (..),
  AmountGuarantees (..),
  PLovelace (..),
  PAssetClass (..),

  -- * Functions

  -- ** Creation

  -- *** PCurrencySymbol
  padaSymbol,
  padaSymbolData,

  -- *** PTokenName
  padaToken,

  -- *** PValue
  psingleton,
  psingletonData,
  pconstantPositiveSingleton,

  -- ** Transformation
  passertPositive,
  passertNonZero,
  passertSorted,
  pforgetPositive,
  pforgetSorted,
  pnormalize,
  padaOnlyValue,
  pnoAdaValue,

  -- ** Partial ordering
  pltPositive,
  pltNonZero,
  pleqPositive,
  pleqNonZero,
  pcheckBinRel,

  -- ** Combination
  pleftBiasedCurrencyUnion,
  pleftBiasedTokenUnion,
  punionWith,
  punionWithData,

  -- ** Queries
  pvalueOf,
  plovelaceValueOf,
  pisAdaOnlyValue,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.AssocMap (PIsAssocMap)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V1.MintValue qualified as V1
import Plutarch.LedgerApi.V3.MintValue qualified as V3
import Plutarch.LedgerApi.Value.CurrencySymbol (PCurrencySymbol, padaSymbol, padaSymbolData)
import Plutarch.LedgerApi.Value.TokenName (PTokenName, padaToken)
import Plutarch.Prelude hiding (psingleton)
import Plutarch.Prelude qualified as PPrelude
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)
import PlutusLedgerApi.V1.Value qualified as PlutusValue
import PlutusLedgerApi.V3 qualified as Plutus
import PlutusTx.Prelude qualified as PlutusTx

-- | @since 2.2.0
newtype PLovelace (s :: S) = PLovelace (Term s PInteger)
  deriving stock
    ( -- | @since 2.2.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 2.2.0
      PIsData
    , -- | @since 2.2.0
      PEq
    , -- | @since 3.3.0
      POrd
    , -- | @since 2.2.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PLovelace)

-- | @since 3.3.0
deriving via
  DeriveNewtypePLiftable PLovelace Plutus.Lovelace
  instance
    PLiftable PLovelace

-- | @since 3.4.0
instance PTryFrom PData (PAsData PLovelace)

-- | @since 2.0.0
data AmountGuarantees = NoGuarantees | NonZero | Positive

----------------------------------------------------------------------
-- PLedgerValue

-- sorted, mandatory ada

-- | @since 3.5.0
newtype PLedgerValue (s :: S)
  = PLedgerValue (Term s (AssocMap.PSortedMap PCurrencySymbol (AssocMap.PSortedMap PTokenName PInteger)))
  deriving stock
    ( -- | @since 3.5.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.5.0
      SOP.Generic
    , -- | @since 3.5.0
      PIsData
    , -- | @since 3.5.0
      PShow
    )
  deriving
    ( -- | @since 3.5.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PLedgerValue)

-- | @since 3.5.0
instance PEq PLedgerValue where
  a #== b = pto a #== pto b

-- | @since 3.5.0
instance Semigroup (Term s PLedgerValue) where
  a <> b = punionWith' # plam (+) # a # b

-- | @since 3.5.0
instance PSemigroup PLedgerValue where
  {-# INLINEABLE (#<>) #-}
  (#<>) = (<>)

{- | Construct a singleton 'PLedgerValue' containing the given quantity of the
given currency, together with a mandatory Ada entry (which may be zero).

@since 3.5.0
-}
psingletonLedgerValue ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PInteger :--> PLedgerValue)
psingletonLedgerValue =
  phoistAcyclic $
    plam $ \symbol token amount ->
      pnormalizeValue @PLedgerValue @PLedgerValue $
        punsafeDowncast
          (AssocMap.psingleton' # symbol #$ AssocMap.psingleton' # token # amount)

----------------------------------------------------------------------
-- PRawValue

-- | @since 3.5.0
newtype PRawValue (s :: S)
  = PRawValue (Term s (AssocMap.PUnsortedMap PCurrencySymbol (AssocMap.PUnsortedMap PTokenName PInteger)))
  deriving stock
    ( -- | @since 3.5.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.5.0
      SOP.Generic
    , -- | @since 3.5.0
      PIsData
    , -- | @since 3.5.0
      PShow
    )
  deriving
    ( -- | @since 3.5.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PRawValue)

{- | Construct an empty 'PRawValue'.

@since 3.5.0
-}
pemptyRawValue :: forall (s :: S). Term s PRawValue
pemptyRawValue = punsafeDowncast AssocMap.pempty'

{- | Construct a singleton 'PRawValue' containing only the given quantity of the
given currency.

@since 3.5.0
-}
psingletonRawValue ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PInteger :--> PRawValue)
psingletonRawValue = phoistAcyclic $
  plam $ \symbol token amount ->
    pif
      (amount #== 0)
      pemptyRawValue
      (punsafeDowncast $ AssocMap.psingleton' # symbol #$ AssocMap.psingleton' # token # amount)

----------------------------------------------------------------------
-- PIsValue, PIsSortedValue

class PIsValue (t :: S -> Type) where
  ptoRawValue :: Term s t -> Term s PRawValue

instance PIsValue PRawValue where
  ptoRawValue = id

instance PIsValue PLedgerValue where
  -- ptoRawValue x = punsafeDowncast $ AssocMap.ptoUnsortedMap $ AssocMap.pmap' # plam AssocMap.ptoUnsortedMap # pto x
  ptoRawValue = punsafeCoerce

instance PIsValue V1.PMintValue where
  ptoRawValue = punsafeCoerce

instance PIsValue V3.PMintValue where
  ptoRawValue = punsafeCoerce

class PIsValue t => PIsSortedValue (t :: S -> Type) where
  ptoSortedMap ::
    Term s t ->
    Term s (AssocMap.PSortedMap PCurrencySymbol (AssocMap.PSortedMap PTokenName PInteger))
  punsafeFromSortedMap ::
    Term s (AssocMap.PSortedMap PCurrencySymbol (AssocMap.PSortedMap PTokenName PInteger)) ->
    Term s t

  -- Can be used to convert any sorted Value to another sorted Value
  pnormalizeValue ::
    forall (t0 :: S -> Type) (s :: S).
    PIsSortedValue t0 =>
    Term s t0 ->
    Term s t

instance PIsSortedValue PLedgerValue where
  ptoSortedMap = pto
  punsafeFromSortedMap = punsafeDowncast

  -- mandatory ada entry
  pnormalizeValue val = pinsertAdaEntry # val

-- V1.PMintValue is also used in V2
instance PIsSortedValue V1.PMintValue where
  ptoSortedMap = pto
  punsafeFromSortedMap = punsafeDowncast

  -- mandatory zero ada entry, non-zero tokens
  pnormalizeValue val = pinsertAdaEntry #$ pnormalizeNoAdaNonZeroTokens @V1.PMintValue # val

instance PIsSortedValue V3.PMintValue where
  ptoSortedMap = pto
  punsafeFromSortedMap = punsafeDowncast

  -- sorted, no ada entry, non-zero tokens
  pnormalizeValue val = pnormalizeNoAdaNonZeroTokens # val

{- | Ensure that the given Value contains an Ada entry.
If missing, a zero Ada entry is inserted at the head of the underlying sorted map.
-}
pinsertAdaEntry ::
  forall (t1 :: S -> Type) (t0 :: S -> Type) (s :: S).
  ( PIsSortedValue t0
  , PIsSortedValue t1
  ) =>
  Term s (t0 :--> t1)
pinsertAdaEntry =
  phoistAcyclic $
    plam $ \value ->
      pmatch (pto $ pto $ ptoSortedMap value) $ \case
        PNil ->
          punsafeFromSortedMap $
            AssocMap.psingleton'
              # padaSymbol
              # (AssocMap.psingleton' # padaToken # 0)
        PCons x xs ->
          pif
            (pfstBuiltin # x #== padaSymbolData)
            (punsafeFromSortedMap $ ptoSortedMap value)
            ( punsafeFromSortedMap $
                punsafeDowncast $
                  punsafeDowncast $
                    pcons
                      # (ppairDataBuiltin # padaSymbolData # pdata (AssocMap.psingleton' # padaToken # 0))
                      # (pcons # x # xs)
            )

{- | Normalize the argument to contain no Ada entries and no zero token
quantities.
-}
pnormalizeNoAdaNonZeroTokens ::
  forall (t1 :: S -> Type) (t0 :: S -> Type) (s :: S).
  ( PIsSortedValue t0
  , PIsSortedValue t1
  ) =>
  Term s (t0 :--> t1)
pnormalizeNoAdaNonZeroTokens = phoistAcyclic $
  plam $ \value ->
    punsafeFromSortedMap $
      AssocMap.pmapMaybeWithKey # plam normalizeTokenMap # ptoSortedMap value
  where
    normalizeTokenMap ::
      forall t' (s' :: S) (k :: S -> Type).
      PIsAssocMap t' =>
      Term s' PCurrencySymbol ->
      Term s' (t' k PInteger) ->
      Term s' (PMaybe (t' k PInteger))
    normalizeTokenMap cs tokenMap =
      pif
        (cs #== padaSymbol)
        (pcon PNothing)
        ( plet (AssocMap.pmapMaybeData' # plam nonZero # tokenMap) $ \normalMap ->
            pif
              (AssocMap.pnull' # normalMap)
              (pcon PNothing)
              (pcon $ PJust normalMap)
        )
    nonZero ::
      forall (s' :: S).
      Term s' (PAsData PInteger) ->
      Term s' (PMaybe (PAsData PInteger))
    nonZero intData =
      pif (intData #== zeroData) (pcon PNothing) (pcon $ PJust intData)

----------------------------------------------------------------------
-- Functions on any 'Value's

{- | Get the quantity of the given currency in the 'PValue'.

@since 2.1.1
-}
pvalueOf' ::
  forall (t :: S -> Type) (s :: S).
  PIsValue t =>
  Term s (t :--> PCurrencySymbol :--> PTokenName :--> PInteger)
pvalueOf' = phoistAcyclic $
  plam $ \value symbol token ->
    AssocMap.pfoldAt'
      # symbol
      # 0
      # plam (\m -> AssocMap.pfoldAt' # token # 0 # plam pfromData # pfromData m)
      # pto (ptoRawValue value)

----------------------------------------------------------------------
-- Functions on sorted 'Value's

{- | Mimics the @lt@ operation on @plutus-ledger-api@'s @Value@.

@since 3.5.0
-}
plt' ::
  forall (t :: S -> Type) (s :: S).
  PIsSortedValue t =>
  PEq t =>
  Term s t ->
  Term s t ->
  Term s PBool
plt' t1 t2 = pleq' t1 t2 #&& (pnot # (t1 #== t2))

{- | Mimics the @leq@ operation on @plutus-ledger-api@'s @Value@.

@since 3.5.0
-}
pleq' ::
  forall (t :: S -> Type) (s :: S).
  PIsSortedValue t =>
  Term s t ->
  Term s t ->
  Term s PBool
pleq' t1 t2 =
  phoistAcyclic (pcheckBinRel' #$ phoistAcyclic $ plam (#<=)) # t1 # t2

{- | Given a description of a relation on amounts, check whether that relation
holds over sorted 'PValue's.

= Important note

This is intended for use with boolean comparison functions, which must define
at least a partial order (total orders and equivalences are acceptable as
well). Use of this with anything else is not guaranteed to give anything
resembling a sensible answer. Use with extreme care.

@since 2.0.0
-}
pcheckBinRel' ::
  forall (t :: S -> Type) (s :: S).
  PIsSortedValue t =>
  Term
    s
    ( (PInteger :--> PInteger :--> PBool)
        :--> t
        :--> t
        :--> PBool
    )
pcheckBinRel' = phoistAcyclic $
  plam $ \f val0 val1 ->
    AssocMap.pcheckBinRel' @PCurrencySymbol
      # (AssocMap.pcheckBinRel' @PTokenName # f # 0)
      # AssocMap.pempty'
      # ptoSortedMap val0
      # ptoSortedMap val1

{- | Combine two sorted 'Value's applying the given function to any pair of
quantities with the same asset class. Note that the result is _not_ 'normalize'd
and may contain zero quantities.

@since 3.5.0
-}
punionWith' ::
  forall (t :: S -> Type) (s :: S).
  PIsSortedValue t =>
  Term
    s
    ( (PInteger :--> PInteger :--> PInteger)
        :--> t
        :--> t
        :--> t
    )
punionWith' = phoistAcyclic $
  plam $ \combine x y ->
    pnormalizeValue $
      punsafeFromSortedMap @t $
        AssocMap.punionWith'
          # plam (\x' y' -> AssocMap.punionWith' # combine # x' # y')
          # ptoSortedMap x
          # ptoSortedMap y

----------------------------------------------------------------------

-- | @since 2.0.0
newtype PValue (keys :: AssocMap.KeyGuarantees) (amounts :: AmountGuarantees) (s :: S)
  = PValue (Term s (AssocMap.PMap keys PCurrencySymbol (AssocMap.PMap keys PTokenName PInteger)))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveNewtypePlutusType (PValue keys amounts))

type role PValue nominal nominal nominal

-- | @since 3.3.0
deriving via
  DeriveNewtypePLiftable
    (PValue 'AssocMap.Unsorted 'NoGuarantees)
    Plutus.Value
  instance
    PLiftable (PValue 'AssocMap.Unsorted 'NoGuarantees)

-- | @since 2.0.0
instance PEq (PValue 'AssocMap.Sorted 'Positive) where
  a #== b = pto a #== pto b

-- | @since 2.0.0
instance PEq (PValue 'AssocMap.Sorted 'NonZero) where
  a #== b = pto a #== pto b

{- | Mimics the @lt@ operation on @plutus-ledger-api@'s @Value@.

@since 3.3.0
-}
pltPositive ::
  forall (s :: S).
  Term s (PValue 'AssocMap.Sorted 'Positive) ->
  Term s (PValue 'AssocMap.Sorted 'Positive) ->
  Term s PBool
pltPositive t1 t2 = pltNonZero (pforgetPositive t1) (pforgetPositive t2)

{- | As 'pltPositive', but for nonzero guaranteed 'PValue's instead.

@since 3.3.0
-}
pltNonZero ::
  forall (s :: S).
  Term s (PValue 'AssocMap.Sorted 'NonZero) ->
  Term s (PValue 'AssocMap.Sorted 'NonZero) ->
  Term s PBool
pltNonZero t1 t2 = pleqNonZero t1 t2 #&& (pnot # (t1 #== t2))

{- | Mimics the @leq@ operation on @plutus-ledger-api@'s @Value@.

@since 3.3.0
-}
pleqPositive ::
  forall (s :: S).
  Term s (PValue 'AssocMap.Sorted 'Positive) ->
  Term s (PValue 'AssocMap.Sorted 'Positive) ->
  Term s PBool
pleqPositive t1 t2 = pleqNonZero (pforgetPositive t1) (pforgetPositive t2)

{- | As 'pletPositive', but for nonzero guaranteed 'PValue's instead.

@since 3.3.0
-}
pleqNonZero ::
  forall (s :: S).
  Term s (PValue 'AssocMap.Sorted 'NonZero) ->
  Term s (PValue 'AssocMap.Sorted 'NonZero) ->
  Term s PBool
pleqNonZero t1 t2 =
  phoistAcyclic (pcheckBinRel #$ phoistAcyclic $ plam (#<=)) # t1 # t2

-- | @since 2.0.0
instance PEq (PValue 'AssocMap.Sorted 'NoGuarantees) where
  a #== b =
    AssocMap.pall
      # (AssocMap.pall # plam (#== 0))
      -- While '(-)' is not commutative, we don't need that property here.
      -- TODO benchmark with '(==)'
      # pto (punionWith # plam (-) # a # b)

-- | @since 3.3.0
instance PSemigroup (PValue 'AssocMap.Sorted 'Positive) where
  {-# INLINEABLE (#<>) #-}
  (#<>) = (<>)

-- | @since 2.0.0
instance Semigroup (Term s (PValue 'AssocMap.Sorted 'Positive)) where
  a <> b =
    punsafeDowncast (pto $ punionWith # plam (+) # a # b)

-- | @since 2.0.0
instance PlutusTx.Semigroup (Term s (PValue 'AssocMap.Sorted 'Positive)) where
  a <> b =
    punsafeDowncast (pto $ punionWith # plam (+) # a # b)

-- | @since 3.3.0
instance PSemigroup (PValue 'AssocMap.Sorted 'NonZero) where
  {-# INLINEABLE (#<>) #-}
  (#<>) = (<>)

-- | @since 2.0.0
instance Semigroup (Term s (PValue 'AssocMap.Sorted 'NonZero)) where
  a <> b =
    pnormalize #$ punionWith # plam (+) # a # b

-- | @since 2.0.0
instance PlutusTx.Semigroup (Term s (PValue 'AssocMap.Sorted 'NonZero)) where
  a <> b =
    pnormalize #$ punionWith # plam (+) # a # b

-- | @since 3.3.0
instance PSemigroup (PValue 'AssocMap.Sorted 'NoGuarantees) where
  {-# INLINEABLE (#<>) #-}
  (#<>) = (<>)

-- | @since 2.0.0
instance Semigroup (Term s (PValue 'AssocMap.Sorted 'NoGuarantees)) where
  a <> b =
    punionWith # plam (+) # a # b

-- | @since 2.0.0
instance PlutusTx.Semigroup (Term s (PValue 'AssocMap.Sorted 'NoGuarantees)) where
  a <> b =
    punionWith # plam (+) # a # b

-- | @since 3.3.0
instance
  PSemigroup (PValue 'AssocMap.Sorted normalization) =>
  PMonoid (PValue 'AssocMap.Sorted normalization)
  where
  {-# INLINEABLE pmempty #-}
  pmempty = pcon (PValue AssocMap.pempty)

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

-- | @since 3.4.0
instance PTryFrom PData (PAsData (PValue 'AssocMap.Unsorted 'NoGuarantees))

-- | @since 3.4.0
instance PTryFrom PData (PAsData (PValue 'AssocMap.Sorted 'NoGuarantees))

-- | @since 3.4.0
instance PTryFrom PData (PAsData (PValue 'AssocMap.Sorted 'Positive)) where
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData (PValue 'AssocMap.Sorted 'NoGuarantees)) opq
    unwrapped <- tcont . plet . papp passertPositive . pfromData $ opq'
    pure (pdata unwrapped, ())

-- | @since 3.4.0
instance PTryFrom PData (PAsData (PValue 'AssocMap.Unsorted 'Positive)) where
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData (PValue 'AssocMap.Unsorted 'NoGuarantees)) opq
    unwrapped <- tcont . plet . papp passertPositive . pfromData $ opq'
    pure (pdata unwrapped, ())

-- | @since 3.4.0
instance PTryFrom PData (PAsData (PValue 'AssocMap.Sorted 'NonZero)) where
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData (PValue 'AssocMap.Sorted 'NoGuarantees)) opq
    unwrapped <- tcont . plet . papp passertNonZero . pfromData $ opq'
    pure (pdata unwrapped, ())

-- | @since 3.4.0
instance PTryFrom PData (PAsData (PValue 'AssocMap.Unsorted 'NonZero)) where
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData (PValue 'AssocMap.Unsorted 'NoGuarantees)) opq
    unwrapped <- tcont . plet . papp passertNonZero . pfromData $ opq'
    pure (pdata unwrapped, ())

-- | @since 3.3.0
newtype PAssetClass (s :: S) = PAssetClass (Term s (PBuiltinPair (PAsData PCurrencySymbol) (PAsData PTokenName)))
  deriving stock
    ( -- | @since 3.3.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 3.3.0
      PIsData
    , -- | @since 3.3.0
      PEq
    , -- | @since 3.3.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PAssetClass)

-- | @since 3.3.0
instance POrd PAssetClass where
  {-# INLINEABLE (#<=) #-}
  ac1 #<= ac2 = pmatch ac1 $ \(PAssetClass pair1) ->
    pmatch ac2 $ \(PAssetClass pair2) ->
      plet (pfromData $ pfstBuiltin # pair1) $ \fst1 ->
        plet (pfromData $ pfstBuiltin # pair2) $ \fst2 ->
          (fst1 #< fst2)
            #|| ( (fst1 #== fst2)
                    #&& let snd1 = pfromData $ psndBuiltin # pair1
                            snd2 = pfromData $ psndBuiltin # pair2
                         in snd1 #<= snd2
                )
  {-# INLINEABLE (#<) #-}
  ac1 #< ac2 = pmatch ac1 $ \(PAssetClass pair1) ->
    pmatch ac2 $ \(PAssetClass pair2) ->
      plet (pfromData $ pfstBuiltin # pair1) $ \fst1 ->
        plet (pfromData $ pfstBuiltin # pair2) $ \fst2 ->
          (fst1 #< fst2)
            #|| ( (fst1 #== fst2)
                    #&& let snd1 = pfromData $ psndBuiltin # pair1
                            snd2 = pfromData $ psndBuiltin # pair2
                         in snd1 #< snd2
                )

-- | @since 3.3.0
deriving via
  DeriveNewtypePLiftable PAssetClass PlutusValue.AssetClass
  instance
    PLiftable PAssetClass

-- | @since 3.4.0
instance PTryFrom PData (PAsData PAssetClass)

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
    punsafeCoerce @(PValue AssocMap.Sorted any0 :--> PValue AssocMap.Sorted any1 :--> PBool) $
      AssocMap.pcheckBinRel @PCurrencySymbol # (AssocMap.pcheckBinRel @PTokenName # f # 0) # AssocMap.pempty

{- | Combine two 'PValue's applying the given function to any pair of
 quantities with the same asset class. Note that the result is _not_
 'normalize'd and may contain zero quantities.

 @since 3.5.0
-}
punionWith ::
  forall (any0 :: AmountGuarantees) (any1 :: AmountGuarantees) (s :: S).
  Term
    s
    ( (PInteger :--> PInteger :--> PInteger)
        :--> PValue 'AssocMap.Sorted any0
        :--> PValue 'AssocMap.Sorted any1
        :--> PValue 'AssocMap.Sorted 'NoGuarantees
    )
punionWith = phoistAcyclic $
  plam $ \combine x y ->
    pcon . PValue $
      AssocMap.punionWith
        # plam (\x' y' -> AssocMap.punionWith # combine # x' # y')
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
      (ptraceInfoError "Negative amount in Value")

{- | Construct a constant singleton 'PValue' containing only the given
positive quantity of the given currency.

@since 2.1.1
-}
pconstantPositiveSingleton ::
  forall (s :: S).
  (forall (s' :: S). Term s' PCurrencySymbol) ->
  (forall (s' :: S). Term s' PTokenName) ->
  (forall (s' :: S). Term s' PInteger) ->
  Term s (PValue 'AssocMap.Sorted 'Positive)
pconstantPositiveSingleton symbol token amount
  | plift amount == 0 = mempty
  | plift amount < 0 = error "Negative amount"
  | otherwise = punsafeDowncast (AssocMap.psingleton # symbol #$ AssocMap.psingleton # token # amount)

{- | Forget the knowledge of all value's guarantees.

@since 2.1.1
-}
pforgetSorted ::
  forall (a :: AmountGuarantees) (k :: AssocMap.KeyGuarantees) (s :: S).
  Term s (PValue 'AssocMap.Sorted a) ->
  Term s (PValue k a)
pforgetSorted = punsafeCoerce

{- | Construct a singleton 'PValue' containing only the given quantity of the
given currency.

@since 2.1.1
-}
psingleton ::
  forall (s :: S).
  Term
    s
    (PCurrencySymbol :--> PTokenName :--> PInteger :--> PValue 'AssocMap.Sorted 'NonZero)
psingleton = phoistAcyclic $
  plam $ \symbol token amount ->
    pif
      (amount #== 0)
      mempty
      (punsafeDowncast $ AssocMap.psingleton # symbol #$ AssocMap.psingleton # token # amount)

{- | Construct a singleton 'PValue' containing only the given quantity of the
 given currency, taking data-encoded parameters.

 @since 2.1.1
-}
psingletonData ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PAsData PInteger
        :--> PValue 'AssocMap.Sorted 'NonZero
    )
psingletonData = phoistAcyclic $
  plam $ \symbol token amount ->
    pif
      (amount #== zeroData)
      mempty
      ( punsafeDowncast
          ( AssocMap.psingletonData
              # symbol
              #$ pdata
              $ AssocMap.psingletonData # token # amount
          )
      )

{- | Get the quantity of the given currency in the 'PValue'.

@since 2.1.1
-}
pvalueOf ::
  forall (anyKey :: AssocMap.KeyGuarantees) (anyAmount :: AmountGuarantees) (s :: S).
  Term s (PValue anyKey anyAmount :--> PCurrencySymbol :--> PTokenName :--> PInteger)
pvalueOf = phoistAcyclic $
  plam $ \value symbol token ->
    AssocMap.pfoldAt
      # symbol
      # 0
      # plam (\m -> AssocMap.pfoldAt # token # 0 # plam pfromData # pfromData m)
      # pto value

{- | Get the amount of Lovelace in the 'PValue'.

@since 2.1.1
-}
plovelaceValueOf ::
  forall (v :: AmountGuarantees) (s :: S).
  Term s (PValue 'AssocMap.Sorted v :--> PInteger)
plovelaceValueOf = phoistAcyclic $
  plam $ \value ->
    pmatch (pto $ pto value) $ \case
      PNil -> 0
      PCons x _ ->
        pif'
          # (pfstBuiltin # x #== padaSymbolData)
          # pfromData (psndBuiltin #$ phead #$ pto $ pfromData $ psndBuiltin # x)
          # 0

{- | Combine two 'PValue's, taking the tokens from the left only, if a
currency occurs on both sides.

@since 2.1.1
-}
pleftBiasedCurrencyUnion ::
  forall (any0 :: AmountGuarantees) (any1 :: AmountGuarantees) (s :: S).
  Term
    s
    ( PValue 'AssocMap.Sorted any0
        :--> PValue 'AssocMap.Sorted any1
        :--> PValue 'AssocMap.Sorted 'NoGuarantees
    )
pleftBiasedCurrencyUnion = phoistAcyclic $
  plam $
    \x y -> pcon . PValue $ AssocMap.pleftBiasedUnion # pto x # pto y

{- | Combine two 'PValue's, taking the tokens from the left only, if a token name
 of the same currency occurs on both sides.

 @since 2.1.1
-}
pleftBiasedTokenUnion ::
  forall (any0 :: AmountGuarantees) (any1 :: AmountGuarantees) (s :: S).
  Term
    s
    ( PValue 'AssocMap.Sorted any0
        :--> PValue 'AssocMap.Sorted any1
        :--> PValue 'AssocMap.Sorted 'NoGuarantees
    )
pleftBiasedTokenUnion = phoistAcyclic $
  plam $ \x y ->
    pcon . PValue $
      AssocMap.punionWith
        # plam (\x' y' -> AssocMap.pleftBiasedUnion # x' # y')
        # pto x
        # pto y

{- | Combine two 'PValue's applying the given function to any pair of
 data-encoded quantities with the same asset class. Note that the result is
 _not_ 'normalize'd and may contain zero quantities.

 @since 3.5.0
-}
punionWithData ::
  forall (any0 :: AmountGuarantees) (any1 :: AmountGuarantees) (s :: S).
  Term
    s
    ( (PAsData PInteger :--> PAsData PInteger :--> PAsData PInteger)
        :--> PValue 'AssocMap.Sorted any0
        :--> PValue 'AssocMap.Sorted any1
        :--> PValue 'AssocMap.Sorted 'NoGuarantees
    )
punionWithData = phoistAcyclic $
  plam $ \combine x y ->
    pcon . PValue $
      AssocMap.punionWith
        # plam (\x' y' -> AssocMap.punionWithData # combine # x' # y')
        # pto x
        # pto y

{- | Assert the value is properly sorted and normalized.

@since 2.1.1
-}
passertSorted ::
  forall (anyKey :: AssocMap.KeyGuarantees) (anyAmount :: AmountGuarantees) (s :: S).
  Term s (PValue anyKey anyAmount :--> PValue 'AssocMap.Sorted 'NonZero)
passertSorted = phoistAcyclic $
  plam $ \value ->
    pif
      ( AssocMap.pany
          # plam
            ( \submap ->
                AssocMap.pnull
                  # (AssocMap.passertSorted # submap)
                  #|| AssocMap.pany
                  # plam (#== 0)
                  # submap
            )
          # pto value
      )
      (ptraceInfoError "Abnormal Value")
      . pcon
      . PValue
      $ AssocMap.passertSorted #$ punsafeCoerce
      $ pto value

{- | Test if the value contains nothing but Ada

@since 2.1.1
-}
pisAdaOnlyValue ::
  forall (s :: S).
  Term s (PValue 'AssocMap.Sorted 'Positive :--> PBool)
pisAdaOnlyValue = phoistAcyclic $
  plam $ \value ->
    pmatch (pto $ pto value) $ \case
      PNil -> pcon PTrue
      PCons x xs -> pand' # (pnull # xs) # (pfstBuiltin # x #== padaSymbolData)

{- | Strip all non-Ada from a 'PValue'.

@since 2.1.1
-}
padaOnlyValue ::
  forall (v :: AmountGuarantees) (s :: S).
  Term s (PValue 'AssocMap.Sorted v :--> PValue 'AssocMap.Sorted v)
padaOnlyValue = phoistAcyclic $
  plam $ \value ->
    pmatch (pto $ pto value) $ \case
      PNil -> value
      PCons x _ ->
        pif'
          # (pfstBuiltin # x #== padaSymbolData)
          # pcon (PValue $ pcon $ AssocMap.PMap $ PPrelude.psingleton # x)
          # pcon (PValue AssocMap.pempty)

{- | Strip all Ada from a 'PValue'.

@since 2.1.1
-}
pnoAdaValue ::
  forall (v :: AmountGuarantees) (s :: S).
  Term s (PValue 'AssocMap.Sorted v :--> PValue 'AssocMap.Sorted v)
pnoAdaValue = phoistAcyclic $
  plam $ \value ->
    pmatch (pto $ pto value) $ \case
      PNil -> value
      PCons x xs -> pif' # (pfstBuiltin # x #== padaSymbolData) # pcon (PValue $ pcon $ AssocMap.PMap xs) # value

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
  pif (outer #$ pto . pto $ val) (punsafeCoerce val) (ptraceInfoError "Zero amount in Value")
  where
    outer ::
      forall (s' :: S) (k :: AssocMap.KeyGuarantees).
      Term s' (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (AssocMap.PMap k PTokenName PInteger))) :--> PBool)
    outer = pfix #$ plam $ \self m ->
      pmatch m $ \case
        PCons x xs -> inner # (pto . pfromData $ psndBuiltin # x) #&& self # xs
        PNil -> pcon PTrue
    inner :: forall (s :: S). Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBool)
    inner = pfix #$ plam $ \self m ->
      pmatch m $ \case
        PCons x xs -> pnot # (psndBuiltin # x #== pconstant @(PAsData PInteger) 0) #&& self # xs
        PNil -> pcon PTrue
