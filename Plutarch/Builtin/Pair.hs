{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Builtin.Pair (
  PPair (..),
  matchPair,
  fstPair,
  sndPair,
  mkPairData,
) where

import Plutarch
import Plutarch.Builtin
import Plutarch.Builtin.Pair.Type
import Plutarch.Prelude

-- This instance is for Data only, because `MkPairData` is the only way to
-- construct a pair. If you want to use a polymorphic pair, use `matchPair`
-- directly.
instance (a ~ POpaque, b ~ POpaque) => PlutusType (PPair a b) where
  type PInner (PPair a b) _ = PPair a b
  pcon' (PPair a b) =
    MkPairData #£ a £ b -- There is no MkPair
  pmatch' = matchPair

matchPair ::
  forall a b s c.
  Term s (PPair a b) ->
  (PPair a b s -> Term s c) ->
  Term s c
matchPair pair f =
  -- TODO: use delay/force to avoid evaluating `pair` twice?
  plet (FstPair #£ pair) $ \a ->
    plet (SndPair #£ pair) $ \b ->
      f $ PPair a b

fstPair :: forall k (s :: k) (a :: k -> Type) (b :: k -> Type). Term s (PPair a b) -> Term s a
fstPair = (FstPair #£)

sndPair :: forall k (s :: k) (a :: k -> Type) (b :: k -> Type). Term s (PPair a b) -> Term s b
sndPair = (SndPair #£)

mkPairData ::
  forall k (s :: k) (a :: k -> Type) (b :: k -> Type).
  (a ~ POpaque, b ~ POpaque) =>
  Term s a ->
  Term s b ->
  Term s (PPair a b)
mkPairData x y = pcon' $ PPair x y
