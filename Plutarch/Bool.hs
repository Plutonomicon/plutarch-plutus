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
) where

import Plutarch (PlutusType (PInner, pcon', pmatch'), punsafeBuiltin)
import Plutarch.Lift (DerivePConstantViaCoercible (DerivePConstantViaCoercible), PConstant, PLifted, PUnsafeLiftDecl, pconstant)
import Plutarch.Prelude
import qualified PlutusCore as PLC

-- | Plutus 'BuiltinBool'
data PBool (s :: S) = PTrue | PFalse

instance PUnsafeLiftDecl PBool where type PLifted PBool = Bool
deriving via (DerivePConstantViaCoercible Bool PBool Bool) instance (PConstant Bool)

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
