{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plutarch.Primitive.Coerce (
  pcoerce,
  pgeneralize,
  pfundamentalize,
  punsafeSpecialize,
  pforgetData,
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, punsafeCoerce)
import Plutarch.Primitive.Data (PAsData, PData)
import Plutarch.Primitive.Representation (
  PCanRepresent,
  PFundamental,
  PRepresentation,
 )

{- | Any Plutarch term can \'forget\' any additional structure it may have, and
revert to being a term of its direct representation. This is safe, and has
zero runtime cost.

@since wip
-}
pcoerce ::
  forall (a :: S -> Type) (s :: S).
  Term s a -> Term s (PRepresentation a)
pcoerce = punsafeCoerce

{- | As 'pcoerce', but can \'look through\' any number of \'layers\' of
representations.

@since wip
-}
pgeneralize ::
  forall (b :: S -> Type) (a :: S -> Type) (s :: S).
  b `PCanRepresent` a => Term s a -> Term s b
pgeneralize = punsafeCoerce

{- | As 'pcoerce', but \'forgets\' /all/ layers of representations.

@since wip
-}
pfundamentalize ::
  forall (a :: S -> Type) (s :: S).
  Term s a -> Term s (PFundamental a)
pfundamentalize = punsafeCoerce

{- | Given a direct representation of some type @a@, declare unconditionally
that it is a term of type @a@. This is not safe: @a@ may impose additional
structure beyond what its direct representation requires, which can break
Plutarch type system guarantees. It is zero runtime cost, and thus exists for
performance: check carefully that you aren't violating any structural
requirements of @a@!

@since wip
-}
punsafeSpecialize ::
  forall (a :: S -> Type) (s :: S).
  Term s (PRepresentation a) -> Term s a
punsafeSpecialize = punsafeCoerce

{- | A type-specialized alias for 'pcoerce'. Included for convenience when we
want to \'forget\' any additional structure stored as 'PAsData'.

@since wip
-}
pforgetData ::
  forall (a :: S -> Type) (s :: S).
  PFundamental a ~ PData => Term s (PAsData a) -> Term s PData
pforgetData = pcoerce
