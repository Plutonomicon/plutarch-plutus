{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Primitive.ByteString (
  -- * Type
  PByteString,
) where

import Data.ByteString (ByteString)
import Plutarch.Backend.S (S)
import Plutarch.Primitive.Apply (
  PlutarchType,
  PlutarchTypeRep (PlutarchTypeRep),
 )
import Plutarch.Primitive.Liftable (
  PLiftable,
  PLiftableDirect (PLiftableDirect),
 )

-- | @since wip
data PByteString (s :: S)

type role PByteString nominal

-- | @since wip
deriving via (PlutarchTypeRep PByteString PByteString) instance PlutarchType PByteString

-- | @since wip
deriving via (PLiftableDirect PByteString ByteString) instance PLiftable PByteString
