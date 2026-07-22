{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Primitive.SOP (
  PSOP,
) where

import Plutarch.Backend.S (S)
import Plutarch.Primitive.Apply (
  PlutarchType,
  PlutarchTypeRep (PlutarchTypeRep),
 )

-- | @since wip
data PSOP (s :: S)

type role PSOP nominal

-- | @since wip
deriving via (PlutarchTypeRep PSOP PSOP) instance PlutarchType PSOP
