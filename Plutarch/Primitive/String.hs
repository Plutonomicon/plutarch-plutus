{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Primitive.String (PString) where

import Plutarch.Backend.S (S)
import Plutarch.Primitive.Apply (PlutarchType, PlutarchTypeRep (PlutarchTypeRep))

-- | @since wip
data PString (s :: S)

type role PString nominal

-- | @since wip
deriving via (PlutarchTypeRep PString PString) instance PlutarchType PString
