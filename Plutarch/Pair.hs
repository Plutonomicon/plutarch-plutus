{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Pair (PPair (..)) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Eq (PEq)
import Plutarch.Internal.PlutusType (PlutusType, pcon, pmatch)
import Plutarch.Internal.Semigroup (
  PMonoid (pmempty),
  PSemigroup (pstimes, (#<>)),
 )
import Plutarch.Internal.Show (PShow)
import Plutarch.Internal.Term (S, Term)
import Plutarch.Repr.SOP (DeriveAsSOPStruct (DeriveAsSOPStruct))

{- |
  Plutus encoding of Pairs.

  Note: This is represented differently than 'BuiltinPair'. It is SoP encoded.
-}
data PPair (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PPair (Term s a) (Term s b)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      SOP.Generic
    , -- | @since WIP
      PEq
    , -- | @since WIP
      PShow
    )

-- | @since WIP
deriving via
  DeriveAsSOPStruct (PPair a b)
  instance
    PlutusType (PPair a b)

-- | @since WIP
instance
  (PSemigroup a, PSemigroup b) =>
  PSemigroup (PPair a b)
  where
  {-# INLINEABLE (#<>) #-}
  x #<> y = pmatch x $ \(PPair x1 x2) ->
    pmatch y $ \(PPair y1 y2) ->
      pcon . PPair (x1 #<> y1) $ (x2 #<> y2)
  {-# INLINEABLE pstimes #-}
  pstimes p x = pmatch x $ \(PPair x1 x2) ->
    pcon . PPair (pstimes p x1) $ pstimes p x2

-- | @since WIP
instance
  (PMonoid a, PMonoid b) =>
  PMonoid (PPair a b)
  where
  {-# INLINEABLE pmempty #-}
  pmempty = pcon . PPair pmempty $ pmempty
