module Plutarch.Builtin.Pair.Type (
  PPair (..),
) where

import Plutarch (Term)

-- | A builtin pair type.
data PPair a b s = PPair (Term s a) (Term s b)
