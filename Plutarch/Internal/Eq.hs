module Plutarch.Internal.Eq (
  PEq (..),
) where

import Data.List.NonEmpty (nonEmpty)
import Generics.SOP (
  All,
  All2,
  HCollapse (hcollapse),
  K (K),
  NP,
  Proxy (Proxy),
  SOP (SOP),
  ccompare_NS,
  hcliftA2,
 )
import Plutarch.Builtin.Bool (
  PBool (PFalse, PTrue),
  pif',
  pnot,
  (#&&),
 )
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Internal.Generic (PCode, PGeneric, gpfrom)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  PlutusType,
  pcon,
  pmatch,
 )
import Plutarch.Internal.Term (
  Term,
  phoistAcyclic,
  plet,
  punsafeBuiltin,
  (#),
  (#$),
  (:-->),
 )
import PlutusCore qualified as PLC

class PEq t where
  (#==) :: Term s t -> Term s t -> Term s PBool
  default (#==) ::
    (PGeneric t, PlutusType t, All2 PEq (PCode t)) =>
    Term s t ->
    Term s t ->
    Term s PBool
  a #== b = gpeq # a # b

infix 4 #==

instance PEq PBool where
  {-# INLINEABLE (#==) #-}
  x #== y' = plet y' $ \y -> pif' # x # y #$ pnot # y

-- | @since WIP
instance PEq PInteger where
  {-# INLINEABLE (#==) #-}
  x #== y = punsafeBuiltin PLC.EqualsInteger # x # y

-- Helpers

-- | Generic version of (#==)
gpeq ::
  forall t s.
  ( PGeneric t
  , PlutusType t
  , All2 PEq (PCode t)
  ) =>
  Term s (t :--> t :--> PBool)
gpeq =
  phoistAcyclic $
    plam $ \x y ->
      pmatch x $ \x' ->
        pmatch y $ \y' ->
          gpeq' (gpfrom x') (gpfrom y')

gpeq' :: All2 PEq xss => SOP (Term s) xss -> SOP (Term s) xss -> Term s PBool
gpeq' (SOP c1) (SOP c2) =
  ccompare_NS (Proxy @(All PEq)) (pcon PFalse) eqProd (pcon PFalse) c1 c2

eqProd :: All PEq xs => NP (Term s) xs -> NP (Term s) xs -> Term s PBool
eqProd p1 p2 =
  pands $ hcollapse $ hcliftA2 (Proxy :: Proxy PEq) eqTerm p1 p2
  where
    eqTerm :: forall s a. PEq a => Term s a -> Term s a -> K (Term s PBool) a
    eqTerm a b =
      K $ a #== b

pands :: [Term s PBool] -> Term s PBool
pands ts' =
  case nonEmpty ts' of
    Nothing -> pcon PTrue
    Just ts -> foldl1 (#&&) ts
