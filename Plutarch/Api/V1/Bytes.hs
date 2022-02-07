{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Bytes (PLedgerBytes (PLedgerBytes)) where

import Plutarch.Lift
import Plutarch.Prelude
import qualified Plutus.V1.Ledger.Bytes as PlutusByte
import qualified PlutusTx.Builtins.Internal as PT

newtype PLedgerBytes (s :: S) = PLedgerBytes (Term s PByteString)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PLedgerBytes PByteString)

instance PUnsafeLiftDecl PLedgerBytes where type PLifted PLedgerBytes = PlutusByte.LedgerBytes
deriving via
  (DerivePConstantViaNewtype PlutusByte.LedgerBytes PLedgerBytes PByteString)
  instance
    (PConstant PlutusByte.LedgerBytes)
