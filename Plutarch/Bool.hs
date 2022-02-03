{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Bool (
  PBool (..),
  PEq (..),
  POrd (..),
  pif,
  pif',
  pnot,
  (#&&),
  (#||),
  por,
  pand,
  pand',
  por',
  -- * Reexport for Booleans
  Boolean(..),
  BooleanOf,
  IfB(..),
  EqB(..),
  OrdB(..),
  boolean,
  minB,
  maxB,
  sort2B,
  guardedB,
  caseB,
) where

import Plutarch.Internal.Other (
  DerivePNewtype,
  PDelayed,
  PlutusType (PInner, pcon', pmatch'),
  S,
  Term,
  pcon,
  pdelay,
  pforce,
  phoistAcyclic,
  plam,
  pmatch,
  pto,
  (#),
  type (:-->),
 )
import Plutarch.Lift (
  DerivePConstantDirect (DerivePConstantDirect),
  PConstant,
  PLifted,
  PUnsafeLiftDecl,
  pconstant,
 )
import Plutarch.Unsafe (punsafeBuiltin)
import qualified PlutusCore as PLC
import Data.Boolean (
  Boolean(..),
  BooleanOf,
  IfB(..),
  EqB(..),
  OrdB(..),
  boolean,
  minB,
  maxB,
  sort2B,
  guardedB,
  caseB,
 )

-- | Plutus 'BuiltinBool'
data PBool (s :: S) = PTrue | PFalse

instance PUnsafeLiftDecl PBool where type PLifted PBool = Bool
deriving via (DerivePConstantDirect Bool PBool) instance (PConstant Bool)

instance Boolean (Term s PBool) where
  true = pconstant True
  false = pconstant False
  (&&*) = (#&&)
  (||*) = (#||)
  notB a = pnot # a

type instance BooleanOf (Term s a) = Term s PBool

instance IfB (Term s a) where
  ifB = pif

instance PEq a => EqB (Term s a) where
  (==*) = (#==)

instance POrd a => OrdB (Term s a) where
  (<*) = (#<)
  (<=*) = (#<=)

instance PlutusType PBool where
  type PInner PBool _ = PBool
  pcon' PTrue = pconstant True
  pcon' PFalse = pconstant False
  pmatch' b f = pforce $ pif' # b # pdelay (f PTrue) # pdelay (f PFalse)

class PEq t where
  (#==) :: Term s t -> Term s t -> Term s PBool

infix 4 #==

class POrd t where
  (#<=) :: Term s t -> Term s t -> Term s PBool
  (#<) :: Term s t -> Term s t -> Term s PBool

infix 4 #<=
infix 4 #<

instance PEq b => PEq (DerivePNewtype a b) where
  x #== y = pto x #== pto y

instance POrd b => POrd (DerivePNewtype a b) where
  x #<= y = pto x #<= pto y
  x #< y = pto x #< pto y

{- | Strict version of 'pif'.
 Emits slightly less code.
-}
pif' :: Term s (PBool :--> a :--> a :--> a)
pif' = phoistAcyclic $ pforce $ punsafeBuiltin PLC.IfThenElse

-- | Lazy if-then-else.
pif :: Term s PBool -> Term s a -> Term s a -> Term s a
pif b case_true case_false = pmatch b $ \case
  PTrue -> case_true
  PFalse -> case_false

-- | Boolean negation for 'PBool' terms.
pnot :: Term s (PBool :--> PBool)
pnot = phoistAcyclic $ plam $ \x -> pif x (pcon PFalse) $ pcon PTrue

-- | Lazily evaluated boolean and for 'PBool' terms.
infixr 3 #&&

(#&&) :: Term s PBool -> Term s PBool -> Term s PBool
x #&& y = pforce $ pand # x # pdelay y

-- | Lazily evaluated boolean or for 'PBool' terms.
infixr 2 #||

(#||) :: Term s PBool -> Term s PBool -> Term s PBool
x #|| y = pforce $ por # x # pdelay y

-- | Hoisted, Plutarch level, lazily evaluated boolean and function.
pand :: Term s (PBool :--> PDelayed PBool :--> PDelayed PBool)
pand = phoistAcyclic $ plam $ \x y -> pif' # x # y # (phoistAcyclic $ pdelay $ pcon PFalse)

-- | Hoisted, Plutarch level, strictly evaluated boolean and function.
pand' :: Term s (PBool :--> PBool :--> PBool)
pand' = phoistAcyclic $ plam $ \x y -> pif' # x # y # (pcon PFalse)

-- | Hoisted, Plutarch level, lazily evaluated boolean or function.
por :: Term s (PBool :--> PDelayed PBool :--> PDelayed PBool)
por = phoistAcyclic $ plam $ \x y -> pif' # x # (phoistAcyclic $ pdelay $ pcon PTrue) # y

-- | Hoisted, Plutarch level, strictly evaluated boolean or function.
por' :: Term s (PBool :--> PBool :--> PBool)
por' = phoistAcyclic $ plam $ \x y -> pif' # x # (pcon PTrue) # y
