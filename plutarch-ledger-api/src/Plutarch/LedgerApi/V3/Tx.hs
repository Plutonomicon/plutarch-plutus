{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.LedgerApi.V3.Tx (
  PTxId (..),
  PTxOutRef (..),
) where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V1.Tx (txIdByteSize)
import Plutarch.Prelude
import PlutusLedgerApi.V3 qualified as Plutus
import PlutusTx.Builtins.Internal qualified as PlutusTx

{- | Hashed with @BLAKE2b-256@.

@since 3.1.0
-}
newtype PTxId (s :: S) = PTxId (Term s PByteString)
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
    via (DeriveNewtypePlutusType PTxId)

-- | @since 3.3.0
instance PLiftable PTxId where
  type AsHaskell PTxId = Plutus.TxId
  type PlutusRepr PTxId = ByteString
  {-# INLINEABLE haskToRepr #-}
  haskToRepr (Plutus.TxId (PlutusTx.BuiltinByteString str)) = str
  {-# INLINEABLE reprToHask #-}
  reprToHask = Right . Plutus.TxId . PlutusTx.BuiltinByteString
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = reprToPlutUni
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = plutToReprUni

-- | @since 3.4.0
instance PTryFrom PData (PAsData PTxId)

{- | Checks that we have a 'PTxId' of valid length. The underlying
'PByteString' must be exactly 32 bytes, as Cardano transactions are hashed
with BLAKE2b-256.

@since wip
-}
instance PValidateData PTxId where
  pwithValidated opq x =
    plet (plengthBS #$ pfromData $ pparseData @PByteString opq) $ \bsSize ->
      pif
        (bsSize #== txIdByteSize)
        x
        perror

-- | @since 3.1.0
data PTxOutRef (s :: S) = PTxOutRef
  { ptxOutRef'id :: Term s (PAsData PTxId)
  , ptxOutRef'idx :: Term s (PAsData PInteger)
  }
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- , -- | @since 3.1.0
      --   POrd

      -- | @since 3.1.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveAsDataStruct PTxOutRef)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PTxOutRef Plutus.TxOutRef
  instance
    PLiftable PTxOutRef

-- | @since 3.4.0
instance PTryFrom PData (PAsData PTxOutRef)

{- | Checks that we have a valid 'PTxOutRef'. The underlying 'PTxId' must be
exactly 32 bytes, as Cardano transactions are hashed with BLAKE2b-256, and
the output index must be a non-negative 'PInteger'.

@since wip
-}
instance PValidateData PTxOutRef where
  pwithValidated opq x =
    pmatch (pasConstr # opq) $ \(PBuiltinPair constrIdx fields) ->
      pif
        ((constrIdx #== 0) #&& ((plength # fields) #== 2))
        ( pwithValidated @PTxId (ptryIndex 0 fields) $
            plet (pasInt # ptryIndex 1 fields) $ \outIdx ->
              pif
                (outIdx #< 0)
                perror
                x
        )
        perror
