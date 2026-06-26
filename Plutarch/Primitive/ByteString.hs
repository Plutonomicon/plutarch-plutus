module Plutarch.Primitive.ByteString (
  -- * Type
  PByteString,
) where

import Plutarch.Backend.S (S)
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))

-- | @since wip
data PByteString (s :: S)

type role PByteString nominal

-- | @since wip
instance PlutarchType PByteString where
  type PRepresentation PByteString = PByteString
