{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.Eq (
  PEq (..),
) where

import Plutarch.Builtin.BLS (
  PBuiltinBLS12_381_G1_Element,
  PBuiltinBLS12_381_G2_Element,
 )
import Plutarch.Builtin.Bool (PBool (PFalse, PTrue), pif, pif', pnot, (#&&))
import Plutarch.Builtin.ByteString (
  PByte,
  PByteString,
  PLogicOpSemantics,
 )
import Plutarch.Builtin.Data (
  PAsData,
  PBuiltinList,
  PBuiltinPair,
  PData,
  pfstBuiltin,
  psndBuiltin,
 )
import Plutarch.Builtin.Integer (PInteger, peqInteger)
import Plutarch.Builtin.String (PString)
import Plutarch.Builtin.Unit (PUnit)

import Data.Kind (Type)
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
import Plutarch.Internal.Fix (pfix)
import Plutarch.Internal.Generic (PCode, PGeneric, gpfrom)
import {-# SOURCE #-} Plutarch.Internal.IsData (PIsData, pdata)
import Plutarch.Internal.Lift (PLiftable (PlutusRepr), pconstant)
import Plutarch.Internal.ListLike (PListLike (pelimList))
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  PlutusType,
  pcon,
  pmatch,
 )
import Plutarch.Internal.Term (
  S,
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

-- | @since WIP
instance PEq PInteger where
  {-# INLINEABLE (#==) #-}
  x #== y = peqInteger # x # y

instance PEq PData where
  x #== y = punsafeBuiltin PLC.EqualsData # x # y

instance PEq (PAsData a) where
  x #== y = punsafeBuiltin PLC.EqualsData # x # y

type family F (a :: S -> Type) :: Bool where
  F PData = 'True
  F (PAsData _) = 'True
  F _ = 'False

class Fc (x :: Bool) (a :: S -> Type) where
  fc :: Proxy x -> Term s (PBuiltinList a) -> Term s (PBuiltinList a) -> Term s PBool

instance (PEq a, PLC.Contains PLC.DefaultUni (PlutusRepr a)) => Fc 'False a where
  fc _ xs ys = plistEquals # xs # ys
    where
      -- TODO: This is copied from ListLike. See if there's a way to not do this
      plistEquals =
        phoistAcyclic $
          pfix #$ plam $ \self xlist ylist ->
            pelimList
              ( \x xs ->
                  pelimList (\y ys -> pif (x #== y) (self # xs # ys) (pconstant False)) (pconstant False) ylist
              )
              (pelimList (\_ _ -> pconstant False) (pconstant True) ylist)
              xlist

instance PIsData (PBuiltinList a) => Fc 'True a where
  fc _ xs ys = pdata xs #== pdata ys

instance Fc (F a) a => PEq (PBuiltinList a) where
  (#==) = fc (Proxy @(F a))

instance (PEq a, PEq b) => PEq (PBuiltinPair a b) where
  p1 #== p2 = pfstBuiltin # p1 #== pfstBuiltin # p2 #&& psndBuiltin # p1 #== psndBuiltin # p2

instance PEq PByteString where
  x #== y = punsafeBuiltin PLC.EqualsByteString # x # y

-- | @since WIP
instance PEq PByte where
  {-# INLINEABLE (#==) #-}
  x #== y = punsafeBuiltin PLC.EqualsInteger # x # y

deriving anyclass instance PEq PLogicOpSemantics

instance PEq PUnit where
  x #== y = plet x \_ -> plet y \_ -> pcon PTrue

instance PEq PString where
  x #== y = punsafeBuiltin PLC.EqualsString # x # y

instance PEq PBuiltinBLS12_381_G1_Element where
  x #== y = punsafeBuiltin PLC.Bls12_381_G1_equal # x # y

instance PEq PBuiltinBLS12_381_G2_Element where
  x #== y = punsafeBuiltin PLC.Bls12_381_G2_equal # x # y
