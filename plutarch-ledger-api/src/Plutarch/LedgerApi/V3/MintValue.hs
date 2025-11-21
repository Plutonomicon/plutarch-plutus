module Plutarch.LedgerApi.V3.MintValue (
  PMintValue,
  pemptyMintValue,
  psingletonMintValue,
  ptoMintValue,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Value (
  PSortedValue,
  pemptySortedValue,
  pforgetSorted,
  phasAdaEntry,
  phasZeroTokenQuantities,
  pnormalizeNoAdaNonZeroTokens,
  psingletonSortedValue,
 )
import Plutarch.LedgerApi.Value.CurrencySymbol (PCurrencySymbol, padaSymbol)
import Plutarch.LedgerApi.Value.TokenName (PTokenName)
import Plutarch.Prelude hiding (psingleton)
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)
import PlutusTx.Prelude qualified as PlutusTx

{- | Represents sorted, well-formed Values without an Ada entry, while all
non-Ada token quantities must be non-zero.

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
  mempty = pemptyMintValue

-- | @since 3.5.0
instance PlutusTx.Monoid (Term s PMintValue) where
  mempty = pemptyMintValue

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

{- | Checks that we have a valid 'PMintValue'. The underlying map must be
sorted, must not include an ADA entry, and must not contain any empty token maps
or tokens with zero quantities.

@since wip
-}
instance PValidateData PMintValue where
  pwithValidated opq x =
    pwithValidated @PSortedValue opq $
      plet (pfromData $ punsafeCoerce @(PAsData PSortedValue) opq) $ \value ->
        pif
          ((phasAdaEntry # value) #|| (phasZeroTokenQuantities # pforgetSorted value))
          perror
          x

{- | Construct an empty 'PMintValue'.

@since wip
-}
pemptyMintValue :: forall (s :: S). Term s PMintValue
pemptyMintValue = punsafeDowncast pemptySortedValue

{- | Construct a singleton 'PMintValue' containing only the given quantity of
the given currency.

= Important note

If the quantity is zero, or if the provided currency symbol is the Ada symbol,
the result is an empty 'PMintValue'.

@since wip
-}
psingletonMintValue ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PInteger :--> PMintValue)
psingletonMintValue =
  phoistAcyclic $
    plam $ \symbol token amount ->
      pif
        (amount #== 0 #|| symbol #== padaSymbol)
        mempty
        ( punsafeDowncast $
            psingletonSortedValue # symbol # token # amount
        )

{- Convert a 'PSortedValue' to a 'PMintValue', ensuring no Ada entry and
non-zero token quantities.

@since 3.5.0
-}
ptoMintValue :: forall (s :: S). Term s (PSortedValue :--> PMintValue)
ptoMintValue = plam (punsafeDowncast . papp pnormalizeNoAdaNonZeroTokens)
