module Plutarch.Api.V1.Maybe (
  PMaybeData (PDJust, PDNothing),
) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic)

import Plutarch.DataRepr (
  PIsDataReprInstances (PIsDataReprInstances),
  PLabeledType ((:=)),
 )
import Plutarch.Prelude

-- | Data encoded Maybe type. Used in various ledger api types.
data PMaybeData a (s :: S)
  = PDJust (Term s (PDataRecord '["_0" ':= a]))
  | PDNothing (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData)
    via PIsDataReprInstances (PMaybeData a)
