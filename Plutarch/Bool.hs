{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
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
) where

import Data.Foldable (foldl')
import Data.SOP.Constraint
import Generics.SOP
import Plutarch.Internal (punsafehoistAcyclic)
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
import Plutarch.Internal.TypeFamily (ToPType2)
import Plutarch.Lift (
  DerivePConstantDirect (DerivePConstantDirect),
  PConstant,
  PLifted,
  PUnsafeLiftDecl,
  pconstant,
 )
import Plutarch.Unsafe (punsafeBuiltin)
import qualified PlutusCore as PLC

-- | Plutus 'BuiltinBool'
data PBool (s :: S) = PTrue | PFalse

instance PUnsafeLiftDecl PBool where type PLifted PBool = Bool
deriving via (DerivePConstantDirect Bool PBool) instance (PConstant Bool)

instance PlutusType PBool where
  type PInner PBool _ = PBool
  pcon' PTrue = pconstant True
  pcon' PFalse = pconstant False
  pmatch' b f = pforce $ pif' # b # pdelay (f PTrue) # pdelay (f PFalse)

class PEq t where
  (#==) :: Term s t -> Term s t -> Term s PBool
  default (#==) ::
    forall s code pcode.
    ( code ~ Code (t s)
    , pcode ~ ToPType2 code
    , Generic (t s)
    , PlutusType t
    , All2 PEq pcode
    , SameShapeAs code pcode
    , SameShapeAs pcode code
    , AllZipF (AllZip (LiftedCoercible I (Term s))) code pcode
    ) =>
    Term s t ->
    Term s t ->
    Term s PBool
  a #== b = peq' # a # b
    where

peq' ::
  forall t s code pcode.
  ( code ~ Code (t s)
  , pcode ~ ToPType2 code
  , Generic (t s)
  , PlutusType t
  , All2 PEq pcode
  , SameShapeAs code pcode
  , SameShapeAs pcode code
  , AllZipF (AllZip (LiftedCoercible I (Term s))) code pcode
  ) =>
  Term s (t :--> t :--> PBool)
peq' =
  punsafehoistAcyclic $
    plam $ \x y ->
      pmatch x $ \x' ->
        pmatch y $ \y' ->
          gpeq @t (pfrom x') (pfrom y')

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

gpeq ::
  forall a s code pcode.
  ( code ~ Code (a s)
  , pcode ~ ToPType2 code
  , All2 PEq pcode
  ) =>
  SOP (Term s) pcode ->
  SOP (Term s) pcode ->
  Term s PBool
gpeq (SOP c1) (SOP c2) =
  ccompare_NS (Proxy @(All PEq)) (pcon PFalse) eqProd (pcon PFalse) c1 c2
  where
    eqProd :: All PEq xs => NP (Term s) xs -> NP (Term s) xs -> Term s PBool
    eqProd p1 p2 =
      foldl' (#&&) (pcon PTrue) $
        hcollapse $ hcliftA2 (Proxy :: Proxy PEq) eqTerm p1 p2
      where
        eqTerm :: forall a. PEq a => Term s a -> Term s a -> K (Term s PBool) a
        eqTerm a b =
          K $ a #== b

{- | Like `from` but for Plutarch terms

  Instead of `I`, this uses `Term s` as the container type.
-}
pfrom ::
  forall a s code pcode.
  ( Generic (a s)
  , code ~ Code (a s)
  , pcode ~ ToPType2 code
  , SameShapeAs code pcode
  , SameShapeAs pcode code
  , AllZipF (AllZip (LiftedCoercible I (Term s))) code pcode
  , All Top pcode
  ) =>
  a s ->
  SOP (Term s) (ToPType2 (Code (a s)))
pfrom = hfromI . from
