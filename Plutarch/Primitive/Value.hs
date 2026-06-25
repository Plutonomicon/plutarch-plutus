{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Primitive.Value (PBValue) where

import Plutarch.Backend.S (S)
import Plutarch.Primitive.Apply (
  PlutarchType,
  PlutarchTypeRep (PlutarchTypeRep),
 )

-- | @since wip
data PBValue (s :: S)

type role PBValue nominal

-- | @since wip
deriving via (PlutarchTypeRep PBValue PBValue) instance PlutarchType PBValue
