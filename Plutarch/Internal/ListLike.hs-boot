{- This can probably be done without hs-boot after dealing with Trace and Show stuff -}
module Plutarch.Internal.ListLike (PIsListLike, PListLike, PElemConstraint, pelimList, pcons, pnil, phead, ptail, pnull) where

import Data.Kind (Constraint, Type)
import Plutarch.Builtin.Bool (PBool, pfalse, ptrue)
import Plutarch.Internal.Term (
  S,
  Term,
  perror,
  phoistAcyclic,
  type (:-->),
 )

-- | 'PIsListLike list a' constraints 'list' be a 'PListLike' with valid element type, 'a'.
type PIsListLike list a = (PListLike list, PElemConstraint list a)

-- | Plutarch types that behave like lists.
class PListLike (list :: (S -> Type) -> S -> Type) where
  type PElemConstraint list (a :: S -> Type) :: Constraint

  -- | Canonical eliminator for list-likes.
  pelimList ::
    PElemConstraint list a =>
    (Term s a -> Term s (list a) -> Term s r) ->
    Term s r ->
    Term s (list a) ->
    Term s r

  -- | Cons an element onto an existing list.
  pcons :: PElemConstraint list a => Term s (a :--> list a :--> list a)

  -- | The empty list
  pnil :: PElemConstraint list a => Term s (list a)

  -- | Return the first element of a list. Partial, throws an error upon encountering an empty list.
  phead :: PElemConstraint list a => Term s (list a :--> a)
  phead = phoistAcyclic $ plam $ pelimList const perror

  -- | Take the tail of a list, meaning drop its head. Partial, throws an error upon encountering an empty list.
  ptail :: PElemConstraint list a => Term s (list a :--> list a)
  ptail = phoistAcyclic $ plam $ pelimList (\_ xs -> xs) perror

  -- | / O(1) /. Check if a list is empty
  pnull :: PElemConstraint list a => Term s (list a :--> PBool)
  pnull = phoistAcyclic $ plam $ pelimList (\_ _ -> pfalse) ptrue
