-- FIXME: This should be its own package as it's not related to Plutarch at all.
module Plutarch.Reducible (Reducible (Reduce)) where

import Data.Coerce (Coercible)
import Data.Functor.Const (Const (Const))
import Data.Functor.Identity (Identity (Identity))

class (Coercible (Reduce x) x) => Reducible (x :: k) where
  type Reduce x :: k

instance Reducible () where type Reduce () = ()

instance Reducible a => Reducible (Const a b) where
  type Reduce (Const a b) = Reduce a

instance Reducible a => Reducible (Identity a) where
  type Reduce (Identity a) = Reduce a
