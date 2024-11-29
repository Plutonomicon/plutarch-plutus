module Plutarch.ByteString (PByteString) where

import Plutarch.Internal.Other (POpaque)
import Plutarch.Internal.Term (Term)

newtype PByteString s = PByteString (Term s POpaque)
