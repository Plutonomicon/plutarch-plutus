module Plutarch.String (PString) where

import Plutarch.Builtin.Opaque (POpaque)
import Plutarch.Internal.Term (Term)

newtype PString s = PString (Term s POpaque)
