{- NOTE: Plutarch's Generic infrastructure is in its fancy, and more
functionality may be added latter.
-}
module Plutarch.Generic (
  GetPDataRecordArgs,
) where

import Plutarch.DataRepr (PDataRecord, PLabeled)
import Plutarch.Prelude

type family GetPDataRecordArgs (a :: [[Type]]) :: [PLabeled] where
  GetPDataRecordArgs '[ '[Term _ (PDataRecord (ps :: [PLabeled]))]] = ps
