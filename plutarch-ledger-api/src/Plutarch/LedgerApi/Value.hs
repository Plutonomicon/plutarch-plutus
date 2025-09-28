module Plutarch.LedgerApi.Value (
  -- * Types
  PRawValue (..),
  PSortedValue,
  PLedgerValue,
  PCurrencySymbol (..),
  PTokenName (..),
  PLovelace (..),
  PAssetClass (..),

  -- * Functions

  -- ** Creation

  -- *** PRawValue
  pemptyRawValue,
  psingletonRawValue,
  psingletonRawValueData,

  -- *** PSortedValue
  pemptySortedValue,
  psingletonSortedValue,
  psingletonSortedValueData,

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
  pforgetSorted,
  ptoLedgerValue,

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
  pmapAmounts,
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

{- | Represents Values without any guarantees.

Values of this type may be unsorted, contain empty token maps, and include
entries with zero token quantities.

@since 3.5.0
-}
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
-- PSortedValue

{- | Represents sorted, well-formed Values without empty token maps.

Compared to 'PRawValue', this type provides stronger guarantees, though
'PSortedValue's may still contain entries with zero token quantities.

@since 3.5.0
-}
newtype PSortedValue (s :: S)
  = PSortedValue (Term s (AssocMap.PSortedMap PCurrencySymbol (AssocMap.PSortedMap PTokenName PInteger)))
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
    via (DeriveNewtypePlutusType PSortedValue)

-- | @since 3.5.0
instance PEq PSortedValue where
  a #== b = pto a #== pto b

-- | @since 3.5.0
instance Semigroup (Term s PSortedValue) where
  a <> b = punionWith # plam (+) # a # b

-- | @since 3.5.0
instance PlutusTx.Semigroup (Term s PSortedValue) where
  a <> b = punionWith # plam (+) # a # b

-- | @since 3.5.0
instance PSemigroup PSortedValue where
  {-# INLINEABLE (#<>) #-}
  (#<>) = (<>)

-- | @since 3.5.0
instance Monoid (Term s PSortedValue) where
  mempty = pemptySortedValue

-- | @since 3.5.0
instance PlutusTx.Monoid (Term s PSortedValue) where
  mempty = pemptySortedValue

-- | @since 3.5.0
instance PMonoid PSortedValue where
  {-# INLINEABLE pmempty #-}
  pmempty = mempty

-- | @since 3.5.0
instance PlutusTx.Group (Term s PSortedValue) where
  inv a = punsafeCoerce $ pmapAmounts # plam negate # pforgetSorted a

-- | @since 3.5.0
instance PTryFrom PData (PAsData PSortedValue) where
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData PRawValue) opq
    unwrapped <- tcont . plet . papp passertSorted . pfromData $ opq'
    pure (pdata unwrapped, ())

----------------------------------------------------------------------
-- PLedgerValue

{- | Represents sorted, well-formed Values with a mandatory Ada entry.

Like 'PSortedValue', but requires the presence of an Ada entry, which may have a
zero quantity. Values of this type may still contain entries with zero token
quantities.

@since 3.5.0
-}
newtype PLedgerValue (s :: S) = PLedgerValue (Term s PSortedValue)
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
  a <> b = punsafeDowncast $ pto a <> pto b

-- | @since 3.5.0
instance PlutusTx.Semigroup (Term s PLedgerValue) where
  a <> b = punsafeDowncast $ pto a <> pto b

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
  inv = punsafeDowncast . PlutusTx.inv . pto

-- | @since 3.5.0
instance PTryFrom PData (PAsData PLedgerValue) where
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData PSortedValue) opq
    unwrapped <- tcont . plet . papp ptoLedgerValue . pfromData $ opq'
    pure (pdata unwrapped, ())

----------------------------------------------------------------------
-- Creation

-- PRawValue --

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

-- PSortedValue --

{- | Construct an empty 'PSortedValue'.

@since 3.5.0
-}
pemptySortedValue :: forall (s :: S). Term s PSortedValue
pemptySortedValue = punsafeDowncast AssocMap.pempty

{- | Construct a singleton 'PSortedValue' containing only the given quantity of
the given currency.

@since 3.5.0
-}
psingletonSortedValue ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PInteger :--> PSortedValue)
psingletonSortedValue =
  phoistAcyclic $
    plam $ \symbol token amount ->
      punsafeDowncast $
        AssocMap.psingleton # symbol #$ AssocMap.psingleton # token # amount

{- | Like 'psingletonSortedValue', but accepts data-encoded arguments.

@since 3.5.0
-}
psingletonSortedValueData ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PAsData PInteger
        :--> PSortedValue
    )
psingletonSortedValueData =
  phoistAcyclic $
    plam $ \symbol token amount ->
      punsafeDowncast $
        AssocMap.psingletonData
          # symbol
          # pdata (AssocMap.psingletonData # token # amount)

-- PLedgerValue --

{- | Construct an empty 'PLedgerValue' with a mandatory zero Ada entry.

@since 3.5.0
-}
pemptyLedgerValue :: forall (s :: S). Term s PLedgerValue
pemptyLedgerValue =
  punsafeDowncast $
    psingletonSortedValue # padaSymbol # padaToken # 0

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
      ptoLedgerValue #$ psingletonSortedValue # symbol # token # amount

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
      ptoLedgerValue #$ psingletonSortedValueData # symbol # token # amount

-----------------------------------------------------------------------
-- Transformation

{- | Attempt to promote a 'PRawValue' to 'PSortedValue'.

The conversion succeeds only if the input Value is already sorted and does not
contain empty token maps. Otherwise, the function fails with an error.

@since 2.1.1
-}
passertSorted :: forall (s :: S). Term s (PRawValue :--> PSortedValue)
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
      ( punsafeDowncast
          -- punsafeCoerce since we know that the token maps are sorted at this point
          (AssocMap.passertSorted #$ punsafeCoerce $ pto value)
      )

{- | Safely demote a 'PSortedValue' to a 'PRawValue'.

@since 3.5.0
-}
pforgetSorted :: forall (s :: S). Term s PSortedValue -> Term s PRawValue
pforgetSorted = punsafeCoerce

{- | Convert a 'PSortedValue' to a 'PLedgerValue', inserting the mandatory Ada
entry if missing.

@since 3.5.0
-}
ptoLedgerValue :: forall (s :: S). Term s (PSortedValue :--> PLedgerValue)
ptoLedgerValue = plam (punsafeDowncast . papp pinsertAdaEntry)

----------------------------------------------------------------------
-- Partial ordering

{- | Mimics the @lt@ operation on @plutus-ledger-api@'s @Value@.

@since 3.5.0
-}
plt :: forall (s :: S). Term s PSortedValue -> Term s PSortedValue -> Term s PBool
plt t1 t2 = pleq t1 t2 #&& (pnot # (t1 #== t2))

{- | Mimics the @leq@ operation on @plutus-ledger-api@'s @Value@.

@since 3.5.0
-}
pleq :: forall (s :: S). Term s PSortedValue -> Term s PSortedValue -> Term s PBool
pleq t1 t2 =
  phoistAcyclic (pcheckBinRel #$ phoistAcyclic $ plam (#<=)) # t1 # t2

{- | Given a description of a relation on amounts, check whether that relation
holds over 'PSortedValue's.

= Important note

This is intended for use with boolean comparison functions, which must define
at least a partial order (total orders and equivalences are acceptable as
well). Use of this with anything else is not guaranteed to give anything
resembling a sensible answer. Use with extreme care.

@since 2.0.0
-}
pcheckBinRel ::
  forall (s :: S).
  Term
    s
    ( (PInteger :--> PInteger :--> PBool)
        :--> PSortedValue
        :--> PSortedValue
        :--> PBool
    )
pcheckBinRel = phoistAcyclic $
  plam $ \f val0 val1 ->
    AssocMap.pcheckBinRel @PCurrencySymbol
      # (AssocMap.pcheckBinRel @PTokenName # f # 0)
      # AssocMap.pempty
      # pto val0
      # pto val1

----------------------------------------------------------------------
-- Combination

{- | Combine two 'PSortedValue's, taking the tokens from the left only, if a
currency occurs on both sides.

@since 2.1.1
-}
pleftBiasedCurrencyUnion :: forall (s :: S). Term s (PSortedValue :--> PSortedValue :--> PSortedValue)
pleftBiasedCurrencyUnion =
  phoistAcyclic $
    plam $ \x y ->
      punsafeDowncast $
        AssocMap.pleftBiasedUnion # pto x # pto y

{- | Combine two 'PSortedValue's, taking the tokens from the left only, if a
token name of the same currency occurs on both sides.

@since 2.1.1
-}
pleftBiasedTokenUnion :: forall (s :: S). Term s (PSortedValue :--> PSortedValue :--> PSortedValue)
pleftBiasedTokenUnion = phoistAcyclic $
  plam $ \x y ->
    punsafeDowncast $
      AssocMap.punionWith
        # plam (\x' y' -> AssocMap.pleftBiasedUnion # x' # y')
        # pto x
        # pto y

{- | Combine two 'PSortedValue's applying the given function to any pair of
quantities with the same asset class.

@since 3.5.0
-}
punionWith ::
  forall (s :: S).
  Term
    s
    ( (PInteger :--> PInteger :--> PInteger)
        :--> PSortedValue
        :--> PSortedValue
        :--> PSortedValue
    )
punionWith =
  phoistAcyclic $
    plam $ \combine x y ->
      punsafeDowncast $
        AssocMap.punionWith
          # plam (\x' y' -> AssocMap.punionWith # combine # x' # y')
          # pto x
          # pto y

{- | Combine two 'PSortedValue's applying the given function to any pair of
data-encoded quantities with the same asset class.

@since 3.5.0
-}
punionWithData ::
  forall (s :: S).
  Term
    s
    ( (PAsData PInteger :--> PAsData PInteger :--> PAsData PInteger)
        :--> PSortedValue
        :--> PSortedValue
        :--> PSortedValue
    )
punionWithData = phoistAcyclic $
  plam $ \combine x y ->
    punsafeDowncast $
      AssocMap.punionWith
        # plam (\x' y' -> AssocMap.punionWithData # combine # x' # y')
        # pto x
        # pto y

----------------------------------------------------------------------
-- Queries

-- TODO: restrict 'pvalueOf' to 'PSortedValue's

{- | Get the quantity of the given currency in the 'PValue'.

@since 2.1.1
-}
pvalueOf :: forall (s :: S). Term s (PRawValue :--> PCurrencySymbol :--> PTokenName :--> PInteger)
pvalueOf = phoistAcyclic $
  plam $ \value symbol token ->
    AssocMap.pfoldAt
      # symbol
      # 0
      # plam (\m -> AssocMap.pfoldAt # token # 0 # plam pfromData # pfromData m)
      # pto value

{- | Get the amount of Lovelace in the 'PSortedValue'.

@since 2.1.1
-}
plovelaceValueOf :: forall (s :: S). Term s (PSortedValue :--> PInteger)
plovelaceValueOf = phoistAcyclic $
  plam $ \value ->
    pmatch (pto $ pto $ pto value) $ \case
      PNil -> 0
      PCons x _ ->
        pif'
          # (pfstBuiltin # x #== padaSymbolData)
          # pfromData (psndBuiltin #$ phead #$ pto $ pto $ pfromData $ psndBuiltin # x)
          # 0

{- | Test if the 'PSortedValue' contains nothing except an Ada entry.

= Note

This function does not verify that Ada is positive and may return 'PTrue'
for zero or negative Ada amounts.

@since 2.1.1
-}
pisAdaOnlyValue :: forall (s :: S). Term s (PSortedValue :--> PBool)
pisAdaOnlyValue = phoistAcyclic $
  plam $ \value ->
    pmatch (pto $ pto $ pto value) $ \case
      PNil -> pcon PTrue
      PCons x xs -> pand' # (pnull # xs) # (pfstBuiltin # x #== padaSymbolData)

----------------------------------------------------------------------
-- Helpers

zeroData :: forall (s :: S). Term s (PAsData PInteger)
zeroData = pdata 0

-- Applies a function to every amount in the map.
pmapAmounts :: forall (s :: S). Term s ((PInteger :--> PInteger) :--> PRawValue :--> PRawValue)
pmapAmounts =
  phoistAcyclic $
    plam $ \f v ->
      punsafeDowncast $
        AssocMap.pmap
          # plam (AssocMap.pmap # f #)
          # pto v

{- | Ensure that the given 'PSortedValue' contains an Ada entry.
If missing, a zero Ada entry is inserted at the head of the underlying sorted map.
-}
pinsertAdaEntry :: forall (s :: S). Term s (PSortedValue :--> PSortedValue)
pinsertAdaEntry =
  phoistAcyclic $
    plam $ \value ->
      pmatch (pto $ pto $ pto value) $ \case
        PNil -> psingletonSortedValue # padaSymbol # padaToken # 0
        PCons x xs ->
          pif
            (pfstBuiltin # x #== padaSymbolData)
            value
            ( punsafeDowncast . punsafeDowncast . punsafeDowncast $
                pcons
                  # (ppairDataBuiltin # padaSymbolData # pdata (AssocMap.psingleton # padaToken # 0))
                  # (pcons # x # xs)
            )

{- | Normalize the argument to contain no Ada entries and no zero token
quantities.
-}
pnormalizeNoAdaNonZeroTokens :: forall (s :: S). Term s (PSortedValue :--> PSortedValue)
pnormalizeNoAdaNonZeroTokens = phoistAcyclic $
  plam $ \value ->
    punsafeDowncast $
      AssocMap.pmapMaybeWithKey # plam normalizeTokenMap # pto value
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
