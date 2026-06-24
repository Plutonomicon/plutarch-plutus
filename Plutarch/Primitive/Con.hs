{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- Needed for safety constraints
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plutarch.Primitive.Con (
  PCon (..),
  pcon,
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, punsafeBuiltin, punsafeCoerce)
import Plutarch.Primitive.Apply (
  PCanRepresent,
  PlutarchType (PRepresentation),
  (#),
 )
import Plutarch.Primitive.Data (PAsData, PData)
import Plutarch.Primitive.Match (PMatch)
import Plutarch.Primitive.Pair (PBPair (PBPair))
import PlutusCore qualified as PLC

{- | = Laws

1. @'pmatch' ('pcon' x) f@ @=@ @f x@
2. @'pmatch' x 'pcon'@ @=@ @x@

@since wip
-}
class PMatch a => PCon (a :: S -> Type) where
  pcon' :: forall (s :: S). a s -> Term s (PRepresentation a)

-- | @since wip
instance
  ( PlutarchType a
  , PlutarchType b
  , PData `PCanRepresent` a
  , PData `PCanRepresent` b
  ) =>
  PCon (PBPair (PAsData a) (PAsData b))
  where
  pcon' (PBPair x y) = punsafeBuiltin PLC.MkPairData # x # y

-- | @since wip
pcon ::
  forall (a :: S -> Type) (s :: S).
  PCon a => a s -> Term s a
pcon = punsafeCoerce . pcon'
