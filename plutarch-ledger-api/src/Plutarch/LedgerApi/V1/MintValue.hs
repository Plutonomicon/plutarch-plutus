module Plutarch.LedgerApi.V1.MintValue (
  PMintValue,
  pempty,
  psingleton,
  ptoMintValue,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Value (
  PSortedValue,
  pforgetSorted,
  phasZeroAdaEntry,
  phasZeroTokenQuantities,
  pinsertAdaEntry,
  pnormalizeNoAdaNonZeroTokens,
  psingletonSortedValue,
 )
import Plutarch.LedgerApi.Value.CurrencySymbol (PCurrencySymbol, padaSymbol, padaSymbolData)
import Plutarch.LedgerApi.Value.TokenName (PTokenName, padaToken)
import Plutarch.Prelude hiding (psingleton)
import Plutarch.Unsafe (punsafeDowncast)
import PlutusTx.Prelude qualified as PlutusTx

{- | Represents sorted, well-formed Values with a mandatory /zero/ Ada entry,
while all other token quantities must be non-zero.

@since 3.5.0
-}
newtype PMintValue (s :: S) = PMintValue (Term s PSortedValue)
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
instance PEq PMintValue where
  a #== b = pto a #== pto b

-- | @since 3.5.0
instance Semigroup (Term s PMintValue) where
  a <> b = ptoMintValue #$ pto a <> pto b

-- | @since 3.5.0
instance PlutusTx.Semigroup (Term s PMintValue) where
  a <> b = ptoMintValue #$ pto a <> pto b

-- | @since 3.5.0
instance PSemigroup PMintValue where
  {-# INLINEABLE (#<>) #-}
  (#<>) = (<>)

-- | @since 3.5.0
instance Monoid (Term s PMintValue) where
  mempty = pempty

-- | @since 3.5.0
instance PlutusTx.Monoid (Term s PMintValue) where
  mempty = pempty

-- | @since 3.5.0
instance PMonoid PMintValue where
  {-# INLINEABLE pmempty #-}
  pmempty = mempty

-- | @since 3.5.0
instance PlutusTx.Group (Term s PMintValue) where
  inv = punsafeDowncast . PlutusTx.inv . pto

-- | @since 3.5.0
instance PTryFrom PData (PAsData PMintValue) where
  ptryFrom' opq = runTermCont $ do
    (opq', _) <- tcont $ ptryFrom @(PAsData PSortedValue) opq
    unwrapped <- tcont . plet . papp ptoMintValue . pfromData $ opq'
    pure (pdata unwrapped, ())

-- | @since wip
instance PValidateData PMintValue where
  pwithValidated opq x =
    plet (pfromData $ pparseData @PSortedValue opq) $ \value ->
      pif
        ((pnot #$ phasZeroAdaEntry # value) #|| (phasZeroTokenQuantities # pforgetSorted value))
        perror
        x

{- | Construct an empty 'PMintValue' with a zero Ada entry.

@since 3.5.0
-}
pempty :: forall (s :: S). Term s PMintValue
pempty = punsafeDowncast $ psingletonSortedValue # padaSymbol # padaToken # 0

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
        -- FIXME: 4 downcasts is ugly
        ( punsafeDowncast . punsafeDowncast . punsafeDowncast . punsafeDowncast $
            pcons
              # (ppairDataBuiltin # padaSymbolData # pdata (AssocMap.psingleton # padaToken # 0))
              # (pcons # (ppairDataBuiltin # pdata symbol #$ pdata (AssocMap.psingleton # token # amount)) # pnil)
        )

{- Convert a 'PSortedValue' to a 'PMintValue', inserting the zero Ada entry
if missing and ensuring non-zero token quantities.

@since 3.5.0
-}
ptoMintValue :: forall (s :: S). Term s (PSortedValue :--> PMintValue)
ptoMintValue =
  phoistAcyclic $
    plam $ \val ->
      punsafeDowncast $
        pinsertAdaEntry #$ pnormalizeNoAdaNonZeroTokens # val
