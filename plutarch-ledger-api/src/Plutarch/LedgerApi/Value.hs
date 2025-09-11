module Plutarch.LedgerApi.Value (
  -- * Types
  PLedgerValue,
  PRawValue (..),
  PCurrencySymbol (..),
  PTokenName (..),
  PLovelace (..),
  PAssetClass (..),

  -- * Classes
  PIsValue (..),
  PIsSortedValue (..),

  -- * Functions

  -- ** Creation

  -- *** PRawValue
  pemptyRawValue,
  psingletonRawValue,
  psingletonRawValueData,

  -- *** PLedgerValue
  pemptyLedgerValue,
  psingletonLedgerValue,
  psingletonLedgerValueData,

  -- *** PCurrencySymbol
  padaSymbol,
  padaSymbolData,

  -- *** PTokenName
  padaToken,

  -- ** Transformation
  passertSorted,

  -- ** Partial ordering
  plt,
  pleq,
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

  -- ** Misc (internal use)
  pinsertAdaEntry,
  pnormalizeNoAdaNonZeroTokens,
  punsafeMapAmounts,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Value.AssetClass (PAssetClass (..))
import Plutarch.LedgerApi.Value.CurrencySymbol (PCurrencySymbol (..), padaSymbol, padaSymbolData)
import Plutarch.LedgerApi.Value.Lovelace (PLovelace (..))
import Plutarch.LedgerApi.Value.TokenName (PTokenName (..), padaToken)
import Plutarch.Prelude hiding (psingleton)
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)
import PlutusLedgerApi.V3 qualified as Plutus
import PlutusTx.Prelude qualified as PlutusTx

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

-- | @since 3.5.0
instance PTryFrom PData (PAsData PRawValue)

-- | @since 3.5.0
deriving via
  DeriveNewtypePLiftable PRawValue Plutus.Value
  instance
    PLiftable PRawValue

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
  a <> b = punsafeUnionWithNonNormalized # plam (+) # a # b

-- | @since 3.5.0
instance PlutusTx.Semigroup (Term s PLedgerValue) where
  a <> b = punsafeUnionWithNonNormalized # plam (+) # a # b

-- | @since 3.5.0
instance PSemigroup PLedgerValue where
  {-# INLINEABLE (#<>) #-}
  (#<>) = (<>)

-- | @since 3.5.0
instance Monoid (Term s PLedgerValue) where
  mempty = pemptyLedgerValue

-- | @since 3.5.0
instance PlutusTx.Monoid (Term s PLedgerValue) where
  mempty = pemptyLedgerValue

-- | @since 3.5.0
instance PMonoid PLedgerValue where
  {-# INLINEABLE pmempty #-}
  pmempty = mempty

-- | @since 3.5.0
instance PlutusTx.Group (Term s PLedgerValue) where
  inv a = punsafeMapAmounts # plam negate # a

-- | @since 3.5.0
instance PTryFrom PData (PAsData PLedgerValue) where
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData PRawValue) opq
    unwrapped <- tcont . plet . papp passertSorted . pfromData $ opq'
    pure (pdata unwrapped, ())

----------------------------------------------------------------------
-- Classes

class PIsValue (t :: S -> Type) where
  ptoRawValue :: Term s t -> Term s PRawValue

instance PIsValue PRawValue where
  ptoRawValue = id

instance PIsValue PLedgerValue where
  -- ptoRawValue x = punsafeDowncast $ AssocMap.ptoUnsortedMap $ AssocMap.pmap' # plam AssocMap.ptoUnsortedMap # pto x
  ptoRawValue = punsafeCoerce

--

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

--

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
            AssocMap.psingleton
              # padaSymbol
              # (AssocMap.psingleton # padaToken # 0)
        PCons x xs ->
          pif
            (pfstBuiltin # x #== padaSymbolData)
            (punsafeFromSortedMap $ ptoSortedMap value)
            ( punsafeFromSortedMap $
                punsafeDowncast $
                  punsafeDowncast $
                    pcons
                      # (ppairDataBuiltin # padaSymbolData # pdata (AssocMap.psingleton # padaToken # 0))
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
      forall
        (t' :: (S -> Type) -> (S -> Type) -> S -> Type)
        (s' :: S)
        (k :: S -> Type).
      PInner (t' k PInteger) ~ AssocMap.PAssocMap k PInteger =>
      Term s' PCurrencySymbol ->
      Term s' (t' k PInteger) ->
      Term s' (PMaybe (t' k PInteger))
    normalizeTokenMap cs tokenMap =
      pif
        (cs #== padaSymbol)
        (pcon PNothing)
        ( plet (AssocMap.pmapMaybeData # plam nonZero # tokenMap) $ \normalMap ->
            pif
              (AssocMap.pnull # punsafeDowncast (pto normalMap))
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
-- `PRawValue` Creation

{- | Construct an empty 'PRawValue'.

@since 3.5.0
-}
pemptyRawValue :: forall (s :: S). Term s PRawValue
pemptyRawValue = punsafeDowncast AssocMap.pempty

{- | Construct a singleton 'PRawValue' containing only the given quantity of the
given currency.

@since 3.5.0
-}
psingletonRawValue ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PInteger :--> PRawValue)
psingletonRawValue = phoistAcyclic $
  plam $ \symbol token amount ->
    punsafeDowncast $
      AssocMap.psingleton # symbol #$ AssocMap.psingleton # token # amount

{- | Construct a singleton 'PRawValue' containing only the given quantity of the
given currency, taking data-encoded parameters.

@since 3.5.0
-}
psingletonRawValueData ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PAsData PInteger
        :--> PRawValue
    )
psingletonRawValueData = phoistAcyclic $
  plam $ \symbol token amount ->
    pif
      (amount #== zeroData)
      pemptyRawValue
      ( punsafeDowncast $
          AssocMap.psingletonData
            # symbol
            # pdata (AssocMap.psingletonData # token # amount)
      )

----------------------------------------------------------------------
-- `PLedgerValue` Creation

{- | Construct an empty 'PLedgerValue' with a mandatory zero Ada entry.

@since 3.5.0
-}
pemptyLedgerValue :: forall (s :: S). Term s PLedgerValue
pemptyLedgerValue =
  punsafeDowncast
    (AssocMap.psingleton # padaSymbol #$ AssocMap.psingleton # padaToken # 0)

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
          (AssocMap.psingleton # symbol #$ AssocMap.psingleton # token # amount)

{- | Like 'psingletonLedgerValue', but accepts data-encoded arguments.

@since 3.5.0
-}
psingletonLedgerValueData ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PAsData PInteger
        :--> PLedgerValue
    )
psingletonLedgerValueData =
  phoistAcyclic $
    plam $ \symbol token amount ->
      pnormalizeValue @PLedgerValue @PLedgerValue $
        punsafeDowncast
          ( AssocMap.psingletonData
              # symbol
              # pdata (AssocMap.psingletonData # token # amount)
          )

-----------------------------------------------------------------------
-- Transformation

{- | Attempt to promote a 'PRawValue' to an instance of 'PIsSortedValue'.

This function verifies that the provided Value is sorted and contains no empty
token maps. If the check succeeds, target-specific normalization is applied;
otherwise, an error is raised.

@since 2.1.1
-}
passertSorted ::
  forall (t :: S -> Type) (s :: S).
  PIsSortedValue t =>
  Term s (PRawValue :--> t)
passertSorted = phoistAcyclic $
  plam $ \value ->
    pif
      ( AssocMap.pany
          # plam
            ( \submap ->
                AssocMap.pnull
                  # AssocMap.pforgetSorted (AssocMap.passertSorted # submap)
            )
          # pto value
      )
      (ptraceInfoError "Abnormal Value")
      ( pnormalizeValue $
          punsafeFromSortedMap @t
            -- punsafeCoerce since we know that the token maps are sorted at this point
            (AssocMap.passertSorted #$ punsafeCoerce $ pto value)
      )

----------------------------------------------------------------------
-- Partial ordering

{- | Mimics the @lt@ operation on @plutus-ledger-api@'s @Value@.

@since 3.5.0
-}
plt ::
  forall (t :: S -> Type) (s :: S).
  PIsSortedValue t =>
  PEq t =>
  Term s t ->
  Term s t ->
  Term s PBool
plt t1 t2 = pleq t1 t2 #&& (pnot # (t1 #== t2))

{- | Mimics the @leq@ operation on @plutus-ledger-api@'s @Value@.

@since 3.5.0
-}
pleq ::
  forall (t :: S -> Type) (s :: S).
  PIsSortedValue t =>
  Term s t ->
  Term s t ->
  Term s PBool
pleq t1 t2 =
  phoistAcyclic (pcheckBinRel #$ phoistAcyclic $ plam (#<=)) # t1 # t2

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
  forall (t :: S -> Type) (s :: S).
  PIsSortedValue t =>
  Term
    s
    ( (PInteger :--> PInteger :--> PBool)
        :--> t
        :--> t
        :--> PBool
    )
pcheckBinRel = phoistAcyclic $
  plam $ \f val0 val1 ->
    AssocMap.pcheckBinRel @PCurrencySymbol
      # (AssocMap.pcheckBinRel @PTokenName # f # 0)
      # AssocMap.pempty
      # ptoSortedMap val0
      # ptoSortedMap val1

----------------------------------------------------------------------
-- Combination

{- | Combine two sorted 'PValue's, taking the tokens from the left only, if a
currency occurs on both sides.

@since 2.1.1
-}
pleftBiasedCurrencyUnion :: forall (t :: S -> Type) (s :: S). PIsSortedValue t => Term s (t :--> t :--> t)
pleftBiasedCurrencyUnion =
  phoistAcyclic $
    plam $ \x y ->
      -- no need to normalize for existing 'PIsSortedValue' instances
      -- normalization is expensive
      punsafeFromSortedMap $
        AssocMap.pleftBiasedUnion # ptoSortedMap x # ptoSortedMap y

{- | Combine two sorted 'PValue's, taking the tokens from the left only, if a
token name of the same currency occurs on both sides.

@since 2.1.1
-}
pleftBiasedTokenUnion :: forall (t :: S -> Type) (s :: S). PIsSortedValue t => Term s (t :--> t :--> t)
pleftBiasedTokenUnion = phoistAcyclic $
  plam $ \x y ->
    -- no need to normalize for existing 'PIsSortedValue' instances
    -- normalization is expensive
    punsafeFromSortedMap $
      AssocMap.punionWith
        # plam (\x' y' -> AssocMap.pleftBiasedUnion # x' # y')
        # ptoSortedMap x
        # ptoSortedMap y

{- | Combine two sorted 'Value's applying the given function to any pair of
quantities with the same asset class. The result is normalized.

@since 3.5.0
-}
punionWith ::
  forall (t :: S -> Type) (s :: S).
  PIsSortedValue t =>
  Term
    s
    ( (PInteger :--> PInteger :--> PInteger)
        :--> t
        :--> t
        :--> t
    )
punionWith = phoistAcyclic $
  plam $ \combine x y ->
    pnormalizeValue $
      punsafeUnionWithNonNormalized # combine # x # y

punsafeUnionWithNonNormalized ::
  forall (t :: S -> Type) (s :: S).
  PIsSortedValue t =>
  Term
    s
    ( (PInteger :--> PInteger :--> PInteger)
        :--> t
        :--> t
        :--> t
    )
punsafeUnionWithNonNormalized = phoistAcyclic $
  plam $ \combine x y ->
    punsafeFromSortedMap @t $
      AssocMap.punionWith
        # plam (\x' y' -> AssocMap.punionWith # combine # x' # y')
        # ptoSortedMap x
        # ptoSortedMap y

{- | Combine two sorted 'PValue's applying the given function to any pair of
data-encoded quantities with the same asset class. The result is normalized.

@since 3.5.0
-}
punionWithData ::
  forall (t :: S -> Type) (s :: S).
  PIsSortedValue t =>
  Term
    s
    ( (PAsData PInteger :--> PAsData PInteger :--> PAsData PInteger)
        :--> t
        :--> t
        :--> t
    )
punionWithData = phoistAcyclic $
  plam $ \combine x y ->
    pnormalizeValue $
      punsafeFromSortedMap @t $
        AssocMap.punionWith
          # plam (\x' y' -> AssocMap.punionWithData # combine # x' # y')
          # ptoSortedMap x
          # ptoSortedMap y

----------------------------------------------------------------------
-- Queries

-- TODO: restrict 'pvalueOf' to 'PIsSortedValue's

{- | Get the quantity of the given currency in the 'PValue'.

@since 2.1.1
-}
pvalueOf ::
  forall (t :: S -> Type) (s :: S).
  PIsValue t =>
  Term s (t :--> PCurrencySymbol :--> PTokenName :--> PInteger)
pvalueOf = phoistAcyclic $
  plam $ \value symbol token ->
    AssocMap.pfoldAt
      # symbol
      # 0
      # plam (\m -> AssocMap.pfoldAt # token # 0 # plam pfromData # pfromData m)
      # pto (ptoRawValue value)

{- | Get the amount of Lovelace in the 'PValue'.

@since 2.1.1
-}
plovelaceValueOf ::
  forall (t :: S -> Type) (s :: S).
  PIsSortedValue t =>
  Term s (t :--> PInteger)
plovelaceValueOf = phoistAcyclic $
  plam $ \value ->
    pmatch (pto $ pto $ ptoSortedMap value) $ \case
      PNil -> 0
      PCons x _ ->
        pif'
          # (pfstBuiltin # x #== padaSymbolData)
          # pfromData (psndBuiltin #$ phead #$ pto $ pto $ pfromData $ psndBuiltin # x)
          # 0

{- | Test if the sorted Value contains nothing except an Ada entry.

= Note

This function does not verify that Ada is positive and may return 'PTrue'
for zero or negative Ada amounts.

@since 2.1.1
-}
pisAdaOnlyValue ::
  forall (t :: S -> Type) (s :: S).
  PIsSortedValue t =>
  Term s (t :--> PBool)
pisAdaOnlyValue = phoistAcyclic $
  plam $ \value ->
    pmatch (pto $ pto $ ptoSortedMap value) $ \case
      PNil -> pcon PTrue
      PCons x xs -> pand' # (pnull # xs) # (pfstBuiltin # x #== padaSymbolData)

----------------------------------------------------------------------
-- Helpers

zeroData :: forall (s :: S). Term s (PAsData PInteger)
zeroData = pdata 0

-- Applies a function to every amount in the map.
-- The resultant Value may be non-normalized.
punsafeMapAmounts ::
  forall (t :: S -> Type) (s :: S).
  PIsValue t =>
  Term s ((PInteger :--> PInteger) :--> t :--> t)
punsafeMapAmounts =
  phoistAcyclic $
    plam $ \f v ->
      punsafeCoerce $
        AssocMap.pmap
          # plam (AssocMap.pmap # f #)
          # pto (ptoRawValue v)
