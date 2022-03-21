-- FIXME: This should be its own package as it's not related to Plutarch at all.
module Plutarch.Reducible (Reducible (Reduce)) where

import Data.Coerce (Coercible)
import Data.Kind (Type)

class (Coercible (Reduce x) x) => Reducible (x :: Type) where
  type Reduce x :: Type

instance Reducible () where type Reduce () = ()
