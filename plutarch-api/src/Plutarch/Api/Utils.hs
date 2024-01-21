{-# LANGUAGE PolyKinds #-}

{- | Useful tools that aren't part of the Plutarch API per se, but get used in
multiple places.
-}
module Plutarch.Api.Utils (
  Mret (..),
) where

import Plutarch.Prelude

{- | 'Term', but with its type arguments flipped. This is a useful helper for
defining 'PTryFrom' instances.

For example, consider the 'PTryFrom' instance for 'PTokenName':

@
instance PTryFrom PData (PAsData PTokenName) where
   type PTryFromExcess PData (PAsData PTokenName) = Mret PTokenName
@

We need to do this because 'PTryFromExcess' expects something of kind @S ->
Type@, but 'Term' has kind @S -> (S -> Type) -> Type@, which doesn't quite
fit. By using 'Mret', we end up with something of kind @(S -> Type)
-> S -> Type@, which fits.

The name is just 'Term' written backwards.

@since 2.0.0
-}
newtype Mret (a :: S -> Type) (s :: S) = Mret (Term s a)
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
