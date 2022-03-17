-- FIXME: This should be its own package as it's not related to Plutarch at all.
module Plutarch.Reducible (Reducible (Reduce)) where

import Data.Coerce (Coercible)
import Data.Functor.Const (Const (Const))
import Data.Kind (Type)

class (Coercible (Reduce x) x) => Reducible (x :: Type) where
  type Reduce x :: Type

instance Reducible () where type Reduce () = ()

instance Reducible x => Reducible (Const x y) where
  type Reduce (Const x y) = Reduce x
