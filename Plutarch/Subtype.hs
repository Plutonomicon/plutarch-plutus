module Plutarch.Subtype (
  -- * Type class
  PSubtype (..),

  -- * Helpers
  ptryFromInfo,
  ptryFromInfo',
  ptryFromDebug,
  ptryFromDebug',

  -- ** Deprecated
  ptryFrom,
) where

import Data.Kind (Type)
import Plutarch.Internal (S, Term)
import Plutarch.String (PString)
import Plutarch.Trace (ptraceDebugError, ptraceInfoError)

{- | = Laws

1. @pdowncast' (pupcast' x) y 'const'@ @=@ @x@

@since 1.6.0
-}
class PSubtype (sub :: S -> Type) (super :: S -> Type) | sub -> super where
  type Leftover sub super :: S -> Type
  pupcast :: forall (s :: S). Term s sub -> Term s super
  pdowncast ::
    forall (r :: S -> Type) (s :: S).
    Term s super ->
    Term s r ->
    (Term s sub -> Term s (Leftover sub super) -> Term s r) ->
    Term s r

-- | @since 1.6.0
ptryFromInfo ::
  forall (sub :: S -> Type) (super :: S -> Type) (r :: S -> Type) (s :: S).
  PSubtype sub super =>
  Term s PString ->
  Term s super ->
  (Term s sub -> Term s (Leftover sub super) -> Term s r) ->
  Term s r
ptryFromInfo msg x = pdowncast x (ptraceInfoError msg)

-- | @since 1.6.0
ptryFromInfo' ::
  forall (sub :: S -> Type) (super :: S -> Type) (s :: S).
  PSubtype sub super =>
  Term s PString ->
  Term s super ->
  Term s sub
ptryFromInfo' msg x = pdowncast x (ptraceInfoError msg) const

-- | @since 1.6.0
ptryFromDebug ::
  forall (sub :: S -> Type) (super :: S -> Type) (r :: S -> Type) (s :: S).
  PSubtype sub super =>
  Term s PString ->
  Term s super ->
  (Term s sub -> Term s (Leftover sub super) -> Term s r) ->
  Term s r
ptryFromDebug msg x = pdowncast x (ptraceDebugError msg)

-- | @since 1.6.0
ptryFromDebug' ::
  forall (sub :: S -> Type) (super :: S -> Type) (s :: S).
  PSubtype sub super =>
  Term s PString ->
  Term s super ->
  Term s sub
ptryFromDebug' msg x = pdowncast x (ptraceDebugError msg) const

{-# DEPRECATED ptryFrom "Use a ptryFrom* which specified log level" #-}

-- | @since 1.5.0
ptryFrom ::
  forall (b :: S -> Type) (a :: S -> Type) (s :: S) (r :: S -> Type).
  PSubtype b a =>
  Term s a ->
  ((Term s b, Term s (Leftover b a)) -> Term s r) ->
  Term s r
ptryFrom x f = pdowncast x (ptraceInfoError "ptryFrom failed") (curry f)
