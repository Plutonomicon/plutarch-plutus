{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Builtin.Data (
  ) where

import Plutarch
import Plutarch.Builtin ((#£))
import qualified Plutarch.Builtin as B
import Plutarch.Builtin.Data.Type

instance PlutusType PData where
  type PInner PData _ = POpaque
  pcon' = \case
    PDataConstr pair ->
      B.ConstrData #£ pair
    _ ->
      undefined
  pmatch' x f =
    B.ChooseData #£ x
      £ f (PDataConstr (B.UnConstrData #£ x))
      £ perror
      £ perror
      £ perror
      £ perror
