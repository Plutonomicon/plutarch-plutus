{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.Subtype (
  PSubtypeRelation (PSubtypeRelation, PNoSubtypeRelation),
  PSubtype,
  PSubtype',
  pupcast,
) where

import Data.Kind (Constraint)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeError (
  ErrorMessage (ShowType, Text, (:<>:)),
  TypeError,
 )
import Plutarch.Internal.PlutusType (
  PlutusType (PInner),
 )
import Plutarch.Internal.Term (PType, Term, punsafeCoerce)
import Plutarch.Internal.Witness (witness)

data PSubtypeRelation
  = PSubtypeRelation
  | PNoSubtypeRelation

type family Helper (a :: PType) (b :: PType) (bi :: PType) :: PSubtypeRelation where
  Helper _ b b = 'PNoSubtypeRelation
  Helper a _ bi = PSubtype' a bi

type family PSubtype' (a :: PType) (b :: PType) :: PSubtypeRelation where
  PSubtype' a a = 'PSubtypeRelation
  PSubtype' a b = Helper a b (PInner b)

{- | @PSubtype a b@ constitutes a subtyping relation between @a@ and @b@.
 This concretely means that `\(x :: Term s b) -> punsafeCoerce x :: Term s a`
 is legal and sound.

 You can not make an instance for this yourself.
 You must use the 'PInner' type family of 'PlutusType' to get this instance.

 Caveat: Only @PInner a POpaque@ is considered unfortunately, as otherwise
 getting GHC to figure out the relation with multiple supertypes is quite hard.

 Subtyping is transitive.
-}
type family PSubtypeHelper (a :: PType) (b :: PType) (r :: PSubtypeRelation) :: Constraint where
  PSubtypeHelper a b 'PNoSubtypeRelation =
    TypeError
      ( 'Text "\""
          ':<>: 'ShowType b
          ':<>: 'Text "\""
          ':<>: 'Text " is not a subtype of "
          ':<>: 'Text "\""
          ':<>: 'ShowType a
          ':<>: 'Text "\""
      )
  PSubtypeHelper _ _ 'PSubtypeRelation = ()

type family PSubtype (a :: PType) (b :: PType) :: Constraint where
  PSubtype a b = (PSubtype' a b ~ 'PSubtypeRelation, PSubtypeHelper a b (PSubtype' a b))

pupcast :: forall a b s. PSubtype a b => Term s b -> Term s a
pupcast = let _ = witness (Proxy @(PSubtype a b)) in punsafeCoerce
