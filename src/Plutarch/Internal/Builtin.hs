-- | Types and functions that are integral to Plutarch, or primitive to UPLC.
module Plutarch.Internal.Builtin (
  -- * Types
  POpaque (..),

  -- * Functions
  popaque,
  pfix,
) where

import Data.Kind (Type)
import Plutarch.Internal.PlutusType (
  PlutusType (
    PContravariant',
    PCovariant',
    PInner,
    PVariant',
    pcon',
    pmatch'
  ),
 )
import Plutarch.Internal.Term (
  S,
  Term,
  phoistAcyclic,
  plam',
  punsafeCoerce,
  (#),
  (:-->),
 )

-- An arbitrary term whose type is unknown.
--
-- @since WIP
newtype POpaque (s :: S) = POpaque (Term s POpaque)

-- | @since WIP
instance PlutusType POpaque where
  type PInner POpaque = POpaque
  type PCovariant' POpaque = ()
  type PContravariant' POpaque = ()
  type PVariant' POpaque = ()
  pcon' (POpaque x) = x
  pmatch' x f = f (POpaque x)

{- | Forget the type of a term.

@since WIP
-}
popaque ::
  forall (a :: S -> Type) (s :: S).
  Term s a ->
  Term s POpaque
popaque = punsafeCoerce

{- |
  Fixpoint recursion. Used to encode recursive functions.

  Example:

  > iterateN' ::
  >  Term s (PInteger :--> (a :--> a) :--> a :--> a) ->
  >  Term s PInteger ->
  >  Term s (a :--> a) ->
  >  Term s a
  > iterateN' self n f x =
  >   pif (n #== 0) x (self # n - 1 #$ f x)
  >
  > iterateN :: Term s (PInteger :--> (a :--> a) :--> a :--> a)
  > iterateN = pfix #$ plam iterateN'
  >

  Further examples can be found in examples/Recursion.hs.

  @since WIP
-}
pfix :: Term s (((a :--> b) :--> a :--> b) :--> a :--> b)
pfix = phoistAcyclic $
  punsafeCoerce $
    plam' $ \f ->
      plam' (\(x :: Term s POpaque) -> f # plam' (\(v :: Term s POpaque) -> punsafeCoerce x # x # v))
        # punsafeCoerce (plam' $ \(x :: Term s POpaque) -> f # plam' (\(v :: Term s POpaque) -> punsafeCoerce x # x # v))
