module Plutarch.Builtin.Integer (PInteger) where

import Data.Kind (Type)
import Plutarch.Internal.Term (S)

type role PInteger phantom
type PInteger :: S -> Type
data PInteger s
