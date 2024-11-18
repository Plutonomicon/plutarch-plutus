module Plutarch.Builtin (PData) where

import Data.Kind (Type)
import Plutarch.Internal (S)

type role PData nominal
type PData :: S -> Type
data PData s
