{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Extra.Monad (tlet, tletField, tmatch, tmatchField) where

import Plutarch.Prelude

import GHC.TypeLits (KnownNat)
import Plutarch.DataRepr (PDataFields (PFields))
import Plutarch.DataRepr.Internal (PLabelIndex, PUnLabel)
import Plutarch.DataRepr.Internal.HList (IndexList)

tlet :: Term s a -> TermCont s (Term s a)
tlet = tcont . plet

tletField ::
  forall name p s a as n.
  ( PDataFields p
  , as ~ PFields p
  , n ~ PLabelIndex name as
  , KnownNat n
  , a ~ PUnLabel (IndexList n as)
  , PIsData a
  ) =>
  Term s p ->
  TermCont s (Term s a)
tletField t = tlet $ pfromData $ pfield @name # t

tmatch :: PMatch a => Term s a -> TermCont s (a s)
tmatch = tcont . pmatch

tmatchField ::
  forall name p s a as n.
  ( PDataFields p
  , as ~ PFields p
  , n ~ PLabelIndex name as
  , KnownNat n
  , a ~ PUnLabel (IndexList n as)
  , PIsData a
  , PMatch a
  ) =>
  Term s p ->
  TermCont s (a s)
tmatchField t = tmatch $ pfromData $ pfield @name # t
