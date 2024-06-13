{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V1 module in plutus-ledger-api
module Plutarch.LedgerApi.Time (
  PPosixTime (..),
) where

import Plutarch.LedgerApi.Utils (Mret)
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Num (PNum)
import Plutarch.Prelude
import Plutarch.Reducible (Reduce)
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V3 qualified as Plutus

-- | @since 2.0.0
newtype PPosixTime (s :: S) = PPosixTime (Term s PInteger)
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
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PIntegral
    , -- | @since 2.0.0
      PNum
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PPosixTime where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 2.0.0
instance PUnsafeLiftDecl PPosixTime where
  type PLifted PPosixTime = Plutus.POSIXTime

-- | @since 2.0.0
deriving via
  (DerivePConstantViaNewtype Plutus.POSIXTime PPosixTime PInteger)
  instance
    PConstantDecl Plutus.POSIXTime

-- | @since 2.0.0
instance PTryFrom PData (PAsData PPosixTime) where
  type PTryFromExcess PData (PAsData PPosixTime) = Mret PPosixTime
  ptryFrom' ::
    forall (s :: S) (r :: S -> Type).
    Term s PData ->
    ((Term s (PAsData PPosixTime), Reduce (PTryFromExcess PData (PAsData PPosixTime) s)) -> Term s r) ->
    Term s r
  ptryFrom' opq = runTermCont $ do
    (wrapped :: Term s (PAsData PInteger), unwrapped :: Term s PInteger) <-
      tcont $ ptryFrom @(PAsData PInteger) opq
    tcont $ \f -> pif (0 #<= unwrapped) (f ()) (ptraceInfoError "ptryFrom(POSIXTime): must be positive")
    pure (punsafeCoerce wrapped, pcon $ PPosixTime unwrapped)
