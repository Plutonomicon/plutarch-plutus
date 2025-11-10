{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.DataRepr.Internal.FromData (PFromDataable, pmaybeFromAsData) where

import Data.Kind (Type)
import Plutarch.Builtin.Data (PAsData)
import Plutarch.Internal.IsData (PIsData, pfromData)
import Plutarch.Internal.Term (S, Term)

{- |
    removes the PAsData if the hole requires it but leaves it
    there if it doesn't

    >>> :t pmaybeFromAsData (pdata 3 :: (Term s (PAsData PInteger))) :: (Term (s::S) PInteger)
    pmaybeFromAsData (pdata 3 :: (Term s (PAsData PInteger))) :: (Term (s::S) PInteger)
    :: forall (s :: S). Term s (PInteger @{S})

    >>> :t pmaybeFromAsData (pdata 3 :: (Term s (PAsData PInteger))) :: (Term (s::S) (PAsData PInteger))
    pmaybeFromAsData (pdata 3 :: (Term s (PAsData PInteger))) :: (Term (s::S) (PAsData PInteger))
    :: forall (s :: S). Term s (PAsData (PInteger @{S}))
-}
class PFromDataable (a :: S -> Type) (b :: S -> Type) | b -> a, a -> b where
  pmaybeFromAsData :: Term s (PAsData a) -> Term s b

instance {-# OVERLAPPABLE #-} PFromDataable a (PAsData a) where
  pmaybeFromAsData = id

instance {-# OVERLAPPABLE #-} (PIsData a, b ~ a) => PFromDataable a b where
  pmaybeFromAsData = pfromData
