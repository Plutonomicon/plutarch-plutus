{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plutarch.Primitive.Data (
  -- * Types
  PData,
  PAsData,
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))

-- | @since wip
data PData (s :: S)

type role PData nominal

-- | @since wip
instance PlutarchType PData where
  type PRepresentation PData = PData

-- | @since wip
data PAsData (a :: S -> Type) (s :: S)

type role PAsData nominal nominal

-- | @since wip
instance PlutarchType (PAsData a) where
  type PRepresentation (PAsData _) = PData
