{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V1 module in plutus-ledger-api
module Plutarch.LedgerApi.V1.Crypto (
  PPubKeyHash (..),
) where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Utils (Mret)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1 qualified as Plutus
import PlutusTx.Builtins.Internal qualified as PlutusTx

-- | @since 2.0.0
newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
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
    via (DeriveNewtypePlutusType PPubKeyHash)

-- | @since 3.3.0
instance PLiftable PPubKeyHash where
  type AsHaskell PPubKeyHash = Plutus.PubKeyHash
  type PlutusRepr PPubKeyHash = ByteString
  {-# INLINEABLE haskToRepr #-}
  haskToRepr (Plutus.PubKeyHash (PlutusTx.BuiltinByteString str)) = str
  {-# INLINEABLE reprToHask #-}
  reprToHask = Right . Plutus.PubKeyHash . PlutusTx.BuiltinByteString
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = reprToPlutUni
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = plutToReprUni

-- | @since 3.3.1
instance PTryFrom PData PPubKeyHash where
  type PTryFromExcess PData PPubKeyHash = Mret PPubKeyHash
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    tcont $ \f ->
      pif
        (plengthBS # unwrapped #== 28)
        (f ())
        (ptraceInfoError "ptryFrom(PPubKeyHash): must be 28 bytes long")
    pure (punsafeCoerce opq, pcon . PPubKeyHash . pcon $ unwrapped)

-- | @since 3.3.1
instance PTryFrom PData (PAsData PPubKeyHash) where
  type PTryFromExcess PData (PAsData PPubKeyHash) = Mret PPubKeyHash
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    tcont $ \f ->
      pif
        (plengthBS # unwrapped #== 28)
        (f ())
        (ptraceInfoError "ptryFrom(PPubKeyHash): must be 28 bytes long")
    pure (punsafeCoerce opq, pcon . PPubKeyHash . pcon $ unwrapped)
