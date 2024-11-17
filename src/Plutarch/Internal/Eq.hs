{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.Internal.Eq (
  -- * Type class
  PEq (..),
) where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Generics.SOP (
  All,
  All2,
  K (K),
  NP,
  SOP (SOP),
  ccompare_NS,
  hcliftA2,
  hcollapse,
 )
import Plutarch.Builtin.Bool (
  PBool (PFalse, PTrue),
  pbuiltinIfThenElse,
 )
import Plutarch.Builtin.ByteString (PByteString, pbuiltinEqualsByteString)
import Plutarch.Builtin.Integer (PInteger, pbuiltinEqualsInteger)
import Plutarch.Builtin.String (PString, pbuiltinEqualsString)
import Plutarch.Builtin.Unit (PUnit)
import Plutarch.Internal.Builtin (
  PBLS12_381_G1_Element,
  PBLS12_381_G2_Element,
  pbuiltinBls12_381_G1_equal,
  pbuiltinBls12_381_G2_equal,
  pnot,
  (#&&),
 )
import Plutarch.Internal.Generic (PCode, PGeneric, gpfrom)
import Plutarch.Internal.PlutusType (PlutusType, pcon, pmatch)
import Plutarch.Internal.Term (S, Term, plet, (#), (#$))

{- | A class for term-level equality.

= Laws

@#==@ must describe an equivalence relation:

1. @x #== x@ @=@ @pcon PTrue@ (reflexivity)
2. @x #== y@ @=@ @y #== x@ (symmetry)
3. @(x #== y) #&& (y #== z)@ @=@ @x #== z@ (transitivity)

Furthermore, equality must imply substitutivity:

4. @x #== y@ @=@ @(f # x) #== (f # y)@

@since WIP
-}
class PEq (a :: S -> Type) where
  (#==) :: forall (s :: S). Term s a -> Term s a -> Term s PBool
  {-# INLINEABLE (#==) #-}
  default (#==) ::
    forall (s :: S).
    (PGeneric a, PlutusType a, All2 PEq (PCode a)) =>
    Term s a ->
    Term s a ->
    Term s PBool
  x #== y =
    pmatch x $ \x' ->
      pmatch y $ \y' ->
        gpeq (gpfrom x') (gpfrom y')

infix 4 #==

-- | @since WIP
instance PEq PUnit where
  {-# INLINEABLE (#==) #-}
  x #== y = plet x $ \_ ->
    plet y $ \_ ->
      pcon PTrue

-- | @since WIP
instance PEq PBLS12_381_G1_Element where
  {-# INLINEABLE (#==) #-}
  x #== y = pbuiltinBls12_381_G1_equal # x # y

-- | @since WIP
instance PEq PBLS12_381_G2_Element where
  {-# INLINEABLE (#==) #-}
  x #== y = pbuiltinBls12_381_G2_equal # x # y

-- | @since WIP
instance PEq PInteger where
  {-# INLINEABLE (#==) #-}
  x #== y = pbuiltinEqualsInteger # x # y

-- | @since WIP
instance PEq PBool where
  {-# INLINEABLE (#==) #-}
  x #== y = plet y $ \y' ->
    pbuiltinIfThenElse # x # y' #$ pnot # y'

-- | @since WIP
instance PEq PString where
  {-# INLINEABLE (#==) #-}
  x #== y = pbuiltinEqualsString # x # y

-- | @since WIP
instance PEq PByteString where
  {-# INLINEABLE (#==) #-}
  x #== y = pbuiltinEqualsByteString # x # y

-- Helpers

gpeq ::
  forall (xss :: [[S -> Type]]) (s :: S).
  All2 PEq xss =>
  SOP (Term s) xss ->
  SOP (Term s) xss ->
  Term s PBool
gpeq (SOP c1) (SOP c2) =
  ccompare_NS (Proxy @(All PEq)) (pcon PFalse) eqProd (pcon PFalse) c1 c2

eqProd ::
  forall (xs :: [S -> Type]) (s :: S).
  All PEq xs =>
  NP (Term s) xs ->
  NP (Term s) xs ->
  Term s PBool
eqProd p1 p2 = pands $ hcollapse $ hcliftA2 (Proxy @PEq) eqTerm p1 p2
  where
    eqTerm ::
      forall (a :: S -> Type) (s' :: S).
      PEq a =>
      Term s' a ->
      Term s' a ->
      K (Term s' PBool) a
    eqTerm x y = K $ x #== y

pands ::
  forall (s :: S).
  [Term s PBool] ->
  Term s PBool
pands = \case
  [] -> pcon PTrue
  xs -> foldl1 (#&&) xs
