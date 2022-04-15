{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Crypto (
  PPubKeyHash (PPubKeyHash),
) where

import qualified Plutus.V1.Ledger.Api as Plutus

import Plutarch.Lift (
  DerivePConstantViaBuiltin (DerivePConstantViaBuiltin),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Prelude
import Plutarch.TryFrom (Flip, PTryFrom (PTryFromExcess, ptryFrom'), ptryFrom)
import Plutarch.Unsafe (punsafeCoerce)

instance PTryFrom PData (PAsData PPubKeyHash) where
  type PTryFromExcess PData (PAsData PPubKeyHash) = Flip Term PPubKeyHash
  ptryFrom' opq = runTermCont $ do
    (wrapped :: Term _ (PAsData PByteString), unwrapped :: Term _ PByteString) <-
      tcont $ ptryFrom @(PAsData PByteString) opq
    tcont $ \f -> pif (plengthBS # unwrapped #== 28) (f ()) (ptraceError "a PubKeyHash should be 28 bytes long")
    pure (punsafeCoerce wrapped, pcon . PPubKeyHash $ unwrapped)

newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PPubKeyHash PByteString)

instance PUnsafeLiftDecl PPubKeyHash where type PLifted PPubKeyHash = Plutus.PubKeyHash
deriving via
  (DerivePConstantViaBuiltin Plutus.PubKeyHash PPubKeyHash PByteString)
  instance
    PConstantDecl Plutus.PubKeyHash
