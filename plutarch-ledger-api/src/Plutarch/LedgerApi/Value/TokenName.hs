module Plutarch.LedgerApi.Value.TokenName (
  PTokenName (..),
  padaToken,
) where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude
import PlutusLedgerApi.V3 qualified as Plutus
import PlutusTx.Builtins.Internal qualified as PlutusTx

-- | @since 2.0.0
newtype PTokenName (s :: S) = PTokenName (Term s PByteString)
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
    via (DeriveNewtypePlutusType PTokenName)

-- Well this is kind of unfortunate, but BuiltinByteString is a thing.

-- | @since 3.3.0
instance PLiftable PTokenName where
  type AsHaskell PTokenName = Plutus.TokenName
  type PlutusRepr PTokenName = ByteString
  {-# INLINEABLE haskToRepr #-}
  haskToRepr (Plutus.TokenName (PlutusTx.BuiltinByteString str)) = str
  {-# INLINEABLE reprToHask #-}
  reprToHask = Right . Plutus.TokenName . PlutusTx.BuiltinByteString
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = reprToPlutUni
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = plutToReprUni

-- | @since 3.4.0
instance PTryFrom PData (PAsData PTokenName)

{- | Checks that we have a 'PTokenName' of valid length. The underlying
'PByteString' must not exceed 32 bytes.

@since wip
-}
instance PValidateData PTokenName where
  pwithValidated opq x =
    plet (plengthBS #$ pasByteStr # opq) $ \bsSize ->
      pif (bsSize #<= ptokenNameByteSizeLimit) x perror

ptokenNameByteSizeLimit :: forall (s :: S). Term s PInteger
ptokenNameByteSizeLimit = 32

{- | The 'PTokenName' of the Ada currency.

@since 2.1.1
-}
padaToken :: Term s PTokenName
padaToken = pconstant Plutus.adaToken
