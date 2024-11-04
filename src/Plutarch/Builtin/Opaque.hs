module Plutarch.Builtin.Opaque (
  -- * Type
  POpaque (..),
) where

import Plutarch.Internal.Term (S, Term)

-- An arbitrary term whose type is unknown.
--
-- @since WIP
newtype POpaque (s :: S) = POpaque (Term s POpaque)
