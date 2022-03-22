{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
  Extra properties based on `prop_haskEquiv`
-}
module Plutarch.Test.Property.Extra (
  prop_leftInverse,
  prop_dataRoundTrip,
) where

import Hedgehog (Gen, Property)

import Plutarch (ClosedTerm)
import Plutarch.Prelude

import Plutarch.Test.Property.HaskEquiv (
  Equality (OnPEq),
  HaskEquiv,
  LamArgs,
  NP (Nil, (:*)),
  Totality (TotalFun),
  prop_haskEquiv,
 )
import Plutarch.Test.Property.Marshal (Marshal)

{- |
  `l` is a left inverse of `r`

  See https://en.wikipedia.org/wiki/Inverse_function#Left_inverses
-}
prop_leftInverse ::
  forall e t p p' h.
  ( LamArgs h ~ '[]
  , HaskEquiv e t (h -> h) (p :--> p) '[h]
  , Show h
  , Marshal h p
  ) =>
  ClosedTerm (p' :--> p) ->
  ClosedTerm (p :--> p') ->
  Gen h ->
  Property
prop_leftInverse l r arg =
  prop_haskEquiv @e @t (id @h) (plam $ \x -> l #$ r # x) (arg :* Nil)

{- |
  A Plutarch term that is a `PIsData` can be encoded to and decoded back to the
  same value.
-}
prop_dataRoundTrip ::
  forall h p.
  ( LamArgs h ~ '[]
  , Show h
  , Marshal h p
  , PIsData p
  , PEq p
  ) =>
  Gen h ->
  Property
prop_dataRoundTrip =
  prop_leftInverse
    @( 'OnPEq)
    @( 'TotalFun)
    @p
    @(PAsData p)
    @h
    (plam pfromData)
    (plam pdata)
