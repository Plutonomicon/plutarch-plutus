module Plutarch.Primitive.ByteString (
  PByteString,
) where

import Plutarch.Backend.Term (S)
import Plutarch.Primitive.Representation (PRepresentation)

-- | @since wip
data PByteString (s :: S)

-- | @since wip
type instance PRepresentation PByteString = PByteString
