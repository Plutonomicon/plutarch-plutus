module Plutarch.Primitive.Function (
  (:-->),
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)

{- | The type of a Plutarch lambda.

@since wip
-}
data (:-->) (a :: S -> Type) (b :: S -> Type) (s :: S)

type role (:-->) nominal nominal nominal

infixr 0 :-->
