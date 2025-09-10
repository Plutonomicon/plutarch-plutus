module Plutarch.LedgerApi.V1.MintValue (
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
  pinsertAdaEntry,
  pnormalizeNoAdaNonZeroTokens,
  punionWith,
  punsafeMapAmounts,
 )
import Plutarch.LedgerApi.Value.CurrencySymbol (PCurrencySymbol, padaSymbol, padaSymbolData)
import Plutarch.LedgerApi.Value.TokenName (PTokenName, padaToken)
import Plutarch.Prelude hiding (psingleton)
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)
import PlutusTx.Prelude qualified as PlutusTx

-- sorted, mandatory zero ada entry, non-zero tokens

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

  -- mandatory zero ada entry, non-zero tokens
  pnormalizeValue val = pinsertAdaEntry #$ pnormalizeNoAdaNonZeroTokens @PMintValue # val

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
  mempty =
    punsafeDowncast $
      AssocMap.psingleton # padaSymbol #$ AssocMap.psingleton # padaToken # 0

-- | @since 3.5.0
instance PlutusTx.Monoid (Term s PMintValue) where
  mempty =
    punsafeDowncast $
      AssocMap.psingleton # padaSymbol #$ AssocMap.psingleton # padaToken # 0

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
the given currency, together with a mandatory zero-Ada entry.


= Important note

If the quantity is zero, or if the provided currency symbol is the Ada symbol,
the result is 'mempty' (i.e. a Value with a single zero-Ada entry).

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
            punsafeDowncast $
              punsafeDowncast $
                pcons
                  # (ppairDataBuiltin # padaSymbolData # pdata (AssocMap.psingleton # padaToken # 0))
                  # (pcons # (ppairDataBuiltin # pdata symbol #$ pdata (AssocMap.psingleton # token # amount)) # pnil)
        )
