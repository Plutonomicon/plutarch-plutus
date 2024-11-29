module Plutarch.Builtin.Data (PData, PAsData) where

import Data.Kind (Type)
import Plutarch.Internal.Term (S)

type role PData nominal
type PData :: S -> Type
data PData s

type role PAsData nominal nominal
type PAsData :: (S -> Type) -> S -> Type
data PAsData a s
