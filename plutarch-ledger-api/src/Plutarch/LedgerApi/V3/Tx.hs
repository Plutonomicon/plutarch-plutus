{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.LedgerApi.V3.Tx (
  PTxId (..),
  PTxOutRef (..),
) where

import Plutarch.Builtin (PDataNewtype (PDataNewtype))
import Plutarch.DataRepr (PDataFields)
import Plutarch.Internal.Lift (DeriveDataPLiftable)
import Plutarch.LedgerApi.Utils (Mret)
import Plutarch.Prelude
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V3 qualified as Plutus

{- | Hashed with @BLAKE2b-256@.

@since 3.1.0
-}
newtype PTxId (s :: S) = PTxId (Term s (PDataNewtype PByteString))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PTxId where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  DeriveDataPLiftable PTxId Plutus.TxId
  instance
    PLiftable PTxId

-- | @since 3.1.0
instance PTryFrom PData PTxId where
  type PTryFromExcess PData PTxId = Mret PTxId
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    tcont $ \f ->
      pif
        (plengthBS # unwrapped #== 32)
        (f ())
        (ptraceInfoError "ptryFrom(PTxId): must be 32 bytes long")
    pure (punsafeCoerce opq, pcon . PTxId . pcon . PDataNewtype . pdata $ unwrapped)

-- | @since 3.1.0
instance PTryFrom PData (PAsData PTxId) where
  type PTryFromExcess PData (PAsData PTxId) = Mret PTxId
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) opq snd
    tcont $ \f ->
      pif
        (plengthBS # unwrapped #== 32)
        (f ())
        (ptraceInfoError "ptryFrom(PTxId): must be 32 bytes long")
    pure (punsafeCoerce opq, pcon . PTxId . pcon . PDataNewtype . pdata $ unwrapped)

-- | @since 3.1.0
newtype PTxOutRef (s :: S)
  = PTxOutRef
      ( Term
          s
          ( PDataRecord
              '[ "id" ':= PTxId
               , "idx" ':= PInteger
               ]
          )
      )
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PDataFields
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      POrd
    , -- | @since 3.1.0
      PTryFrom PData
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PTxOutRef where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PTxOutRef Plutus.TxOutRef
  instance
    PLiftable PTxOutRef

-- | @since 3.1.0
instance PTryFrom PData (PAsData PTxOutRef)
