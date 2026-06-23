module Plutarch.Primitive.Function (
  (:-->),
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Primitive.Representation (PRepresentation)

{- | The type of a Plutarch lambda.

@since wip
-}
data (:-->) (a :: S -> Type) (b :: S -> Type) (s :: S)

infixr 0 :-->

-- | @since wip
type instance PRepresentation (a :--> b) = (a :--> PRepresentation b)
