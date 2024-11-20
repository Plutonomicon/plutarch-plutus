module Plutarch.String (PString) where

import Plutarch.Internal.Other (POpaque)
import Plutarch.Internal.Term (Term)

newtype PString s = PString (Term s POpaque)
