{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.IsData where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))

import {-# SOURCE #-} Plutarch.Builtin.Data
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PlutusType
import Plutarch.Internal.Term (S, Term)
import Plutarch.Internal.Witness (witness)
import Plutarch.TryFrom
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)

{- | Laws:
 - If @PSubtype PData a@, then @pdataImpl a@ must be `pupcast`.
 - pdataImpl . pupcast . pfromDataImpl ≡ id
 - pfromDataImpl . punsafeDowncast . pdataImpl ≡ id
-}
class PIsData a where
  pfromDataImpl :: Term s (PAsData a) -> Term s a
  default pfromDataImpl :: PIsData (PInner a) => Term s (PAsData a) -> Term s a
  pfromDataImpl x = punsafeDowncast $ pfromDataImpl (punsafeCoerce x :: Term _ (PAsData (PInner a)))

  pdataImpl :: Term s a -> Term s PData
  default pdataImpl :: PIsData (PInner a) => Term s a -> Term s PData
  pdataImpl x = pdataImpl $ pto x

pfromData :: PIsData a => Term s (PAsData a) -> Term s a
pfromData = pfromDataImpl

pdata :: PIsData a => Term s a -> Term s (PAsData a)
pdata = punsafeCoerce . pdataImpl

pforgetData :: forall s a. Term s (PAsData a) -> Term s PData
pforgetData = punsafeCoerce

-- FIXME: remove, broken

{- | Like 'pforgetData', except it works for complex types.
 Equivalent to 'pupcastF'.
-}
pforgetData' ::
  forall a (p :: (S -> Type) -> S -> Type) (s :: S).
  PCovariant p =>
  Proxy p ->
  Term s (p (PAsData a)) ->
  Term s (p PData)
pforgetData' _ = let _ = witness (Proxy @(PCovariant p)) in punsafeCoerce

-- | Inverse of 'pforgetData''.
prememberData ::
  forall (p :: (S -> Type) -> S -> Type) (s :: S).
  PVariant p =>
  Proxy p ->
  Term s (p PData) ->
  Term s (p (PAsData PData))
prememberData Proxy = let _ = witness (Proxy @(PVariant p)) in punsafeCoerce

-- | Like 'prememberData' but generalised.
prememberData' ::
  forall a (p :: (S -> Type) -> S -> Type) (s :: S).
  (PSubtype PData a, PVariant p) =>
  Proxy p ->
  Term s (p a) ->
  Term s (p (PAsData a))
prememberData' Proxy = let _ = witness (Proxy @(PSubtype PData a, PVariant p)) in punsafeCoerce
