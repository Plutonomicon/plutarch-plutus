{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Primitive.String (PString) where

import Data.Text (Text)
import Plutarch.Backend.S (S)
import Plutarch.Primitive.Apply (PlutarchType, PlutarchTypeRep (PlutarchTypeRep))
import Plutarch.Primitive.Liftable (
  PLiftable,
  PLiftableDirect (PLiftableDirect),
 )

-- | @since wip
data PString (s :: S)

type role PString nominal

-- | @since wip
deriving via (PlutarchTypeRep PString PString) instance PlutarchType PString

-- | @since wip
deriving via (PLiftableDirect PString Text) instance PLiftable PString
