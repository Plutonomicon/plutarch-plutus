module Plutarch.LedgerApi.Value.CurrencySymbol (
  PCurrencySymbol (..),
  padaSymbol,
  padaSymbolData,
) where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude
import PlutusLedgerApi.V3 qualified as Plutus
import PlutusTx.Builtins.Internal qualified as PlutusTx

-- | @since 2.0.0
newtype PCurrencySymbol (s :: S) = PCurrencySymbol (Term s PByteString)
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
      PEq
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PCurrencySymbol)

-- | @since 3.3.0
instance PLiftable PCurrencySymbol where
  type AsHaskell PCurrencySymbol = Plutus.CurrencySymbol
  type PlutusRepr PCurrencySymbol = ByteString
  {-# INLINEABLE haskToRepr #-}
  haskToRepr (Plutus.CurrencySymbol (PlutusTx.BuiltinByteString str)) = str
  {-# INLINEABLE reprToHask #-}
  reprToHask = Right . Plutus.CurrencySymbol . PlutusTx.BuiltinByteString
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = reprToPlutUni
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = plutToReprUni

-- | @since 3.4.0
instance PTryFrom PData (PAsData PCurrencySymbol)

-- | @since wip
instance PValidateData PCurrencySymbol where
  pwithValidated opq x =
    plet (plengthBS #$ pasByteStr # opq) $ \bsSize ->
      pif
        ((bsSize #== pnonAdaCurrencySymbolByteSize) #|| (bsSize #== padaCurrencySymbolByteSize))
        x
        perror

pnonAdaCurrencySymbolByteSize :: forall (s :: S). Term s PInteger
pnonAdaCurrencySymbolByteSize = 28

padaCurrencySymbolByteSize :: forall (s :: S). Term s PInteger
padaCurrencySymbolByteSize = 0

{- | The 'PCurrencySymbol' of the Ada currency.

@since 2.1.1
-}
padaSymbol :: forall (s :: S). Term s PCurrencySymbol
padaSymbol = pconstant Plutus.adaSymbol

{- | Data-encoded 'PCurrencySymbol' of the Ada currency.

@since 2.1.1
-}
padaSymbolData :: forall (s :: S). Term s (PAsData PCurrencySymbol)
padaSymbolData = pdata padaSymbol
