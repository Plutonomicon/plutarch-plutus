module Plutarch.LedgerApi.V3.MintValue (
  PMintValue,
  psingleton,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Value (
  PIsSortedValue (..),
  PIsValue (..),
  PRawValue,
  passertSorted,
  pnormalizeNoAdaNonZeroTokens,
  punionWith,
  punsafeMapAmounts,
 )
import Plutarch.LedgerApi.Value.CurrencySymbol (PCurrencySymbol, padaSymbol)
import Plutarch.LedgerApi.Value.TokenName (PTokenName)
import Plutarch.Prelude hiding (psingleton)
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)
import PlutusTx.Prelude qualified as PlutusTx

-- sorted, no ada entry, non-zero tokens

-- | @since 3.5.0
newtype PMintValue (s :: S)
  = PMintValue (Term s (AssocMap.PSortedMap PCurrencySymbol (AssocMap.PSortedMap PTokenName PInteger)))
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
    via (DeriveNewtypePlutusType PMintValue)

-- | @since 3.5.0
instance PIsValue PMintValue where
  -- ptoRawValue x = punsafeDowncast $ AssocMap.ptoUnsortedMap $ AssocMap.pmap' # plam AssocMap.ptoUnsortedMap # pto x
  ptoRawValue = punsafeCoerce

-- | @since 3.5.0
instance PIsSortedValue PMintValue where
  ptoSortedMap = pto
  punsafeFromSortedMap = punsafeDowncast

  -- no ada entry, non-zero tokens
  pnormalizeValue val = pnormalizeNoAdaNonZeroTokens # val

-- | @since 3.5.0
instance PEq PMintValue where
  a #== b = pto a #== pto b

-- | @since 3.5.0
instance Semigroup (Term s PMintValue) where
  a <> b = punionWith # plam (+) # a # b

-- | @since 3.5.0
instance PlutusTx.Semigroup (Term s PMintValue) where
  a <> b = punionWith # plam (+) # a # b

-- | @since 3.5.0
instance PSemigroup PMintValue where
  {-# INLINEABLE (#<>) #-}
  (#<>) = (<>)

-- | @since 3.5.0
instance Monoid (Term s PMintValue) where
  mempty = punsafeDowncast AssocMap.pempty

-- | @since 3.5.0
instance PlutusTx.Monoid (Term s PMintValue) where
  mempty = punsafeDowncast AssocMap.pempty

-- | @since 3.5.0
instance PMonoid PMintValue where
  {-# INLINEABLE pmempty #-}
  pmempty = mempty

-- | @since 3.5.0
instance PlutusTx.Group (Term s PMintValue) where
  inv a = punsafeMapAmounts # plam negate # a

-- | @since 3.5.0
instance PTryFrom PData (PAsData PMintValue) where
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData PRawValue) opq
    unwrapped <- tcont . plet . papp passertSorted . pfromData $ opq'
    pure (pdata unwrapped, ())

{- | Construct a singleton 'PMintValue' containing only the given quantity of
the given currency.

= Important note

If the quantity is zero, or if the provided currency symbol is the Ada symbol,
the result is an empty 'PMintValue'.

@since 3.5.0
-}
psingleton ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PInteger :--> PMintValue)
psingleton =
  phoistAcyclic $
    plam $ \symbol token amount ->
      pif
        (amount #== 0 #|| symbol #== padaSymbol)
        mempty
        ( punsafeDowncast $
            AssocMap.psingleton # symbol #$ AssocMap.psingleton # token # amount
        )
