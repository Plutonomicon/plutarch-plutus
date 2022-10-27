module Plutarch.String (PString) where

import Plutarch.Internal (Term)
import Plutarch.Internal.Other (POpaque)

newtype PString s = PString (Term s POpaque)
