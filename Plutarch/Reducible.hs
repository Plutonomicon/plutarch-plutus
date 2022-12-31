{-# LANGUAGE UndecidableInstances #-}

-- FIXME: This should be its own package as it's not related to Plutarch at all.
module Plutarch.Reducible (Reduce, NoReduce (..), reduce) where

import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import GHC.Generics (C1, D1, Rec0, Rep, S1, pattern MetaData)
import Plutarch.Internal (Term)

newtype NoReduce a = NoReduce a

type family GReduce (def :: Type) (rep :: Type -> Type) :: Type where
  -- newtype
  GReduce _ (D1 ('MetaData _ _ _ 'True) (C1 _ (S1 _ (Rec0 (x :: Type))))) = Reduce x
  -- data
  GReduce def _ = def

{- | This class provides a work-around for partially applying
 type families of kind @a@, where @a@ is either 'Type' or
 @b -> c@ where @c@ satisfies the same constraint.

 Given a type family @F : A -> Type@, you can make the following
 @
   type F' :: A -> Type
   newtype F' (a :: A) = F' (NoReduce (F a)) deriving stock Generic
 @
 It is then true that @forall a. Reduce (F' a) ~ F a@.
-}
type family Reduce (x :: Type) :: Type where
  Reduce (NoReduce a) = a
  Reduce (Term s a) = Term s a -- FIXME remove
  Reduce (a -> b) = a -> b
  Reduce x = GReduce x (Rep x)

reduce :: Coercible a (Reduce a) => a -> Reduce a
reduce = coerce
