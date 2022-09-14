{-# Language UndecidableSuperClasses #-}
{-# Language UndecidableInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -w -Wno-orphans #-}

module Plutarch.Bool2 where

import Data.List.NonEmpty (nonEmpty)
-- import Generics.SOP (
--   All,
--   All2,
--   HCollapse (hcollapse),
--   K (K),
--   NP,
--   Proxy (Proxy),
--   SOP (SOP),
--   ccompare_NS,
--   hcliftA2,
--  )
-- import Plutarch.Internal (
--   PDelayed,
--   S,
--   Term,
--   pdelay,
--   pforce,
--   phoistAcyclic,
--   plet,
--   (#),
--   (#$),
--   (#->),
--  )
import Data.Kind (Type, Constraint)

import Plutarch.Core (Term)
import Plutarch.Core
import Plutarch.PType
-- import Plutarch.Internal.Generic (PCode, PGeneric, gpfrom)
-- import Plutarch.Internal.Other (
--   pto,
--  )
-- import Plutarch.Internal.PLam (plam)
import GHC.Generics
import Plutarch.Lam
import Plutarch.Internal2
import Plutarch.Internal.PlutusType2
import Plutarch.Plutus
-- import Plutarch.Internal.PlutusType (PInner, PlutusType, pcon, pcon', pmatch, pmatch')
import Plutarch.Lift2 (
  DerivePConstantDirect (DerivePConstantDirect),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
  pconstant,
 )
import Plutarch.Unsafe2 (punsafeBuiltin)

type PBoolCls s = (PPlutus' s, PIfThenElse s, PConstructable s PBool)

-- | Plutus 'BuiltinBool'

instance PConstructable' Impl' PBool where
  pconImpl :: PConcrete Impl' PBool -> Impl PBool
  pconImpl = undefined 

  pmatchImpl :: Impl PBool -> (PConcrete Impl' PBool -> Term Impl' b) -> Term Impl' b
  pmatchImpl = undefined 

instance PUnsafeLiftDecl PBool where type PLifted PBool = Bool
deriving via DerivePConstantDirect Bool PBool instance PConstantDecl Bool

instance PlutusType PBool where
  type PInner PBool = PBool
--   pcon' PTrue = pconstant True
--   pcon' PFalse = pconstant False
--   pmatch' b f = pforce $ pif' # b # pdelay (f PTrue) # pdelay (f PFalse)

-- instance PConstructable' ('PDSLKind (Term edsl)) PBool where
--   -- pconImpl :: PConcrete Impl' PBool -> Impl PBool
--   pconImpl PFalse = undefined 
--   pconImpl PTrue  = undefined 

  -- pmatchImpl :: Impl PBool -> (PConcrete Impl' PBool -> Term Impl' b) -> Term Impl' b
  -- pmatchImpl (Impl impl) next = undefined 

-- instance PConstructable' Impl' PBool where
--   pconImpl :: PConcrete Impl' PBool -> Impl PBool
--   pconImpl PFalse = pconstant True
--   pconImpl PTrue  = pconstant False

--   pmatchImpl :: Impl PBool -> (PConcrete Impl' PBool -> Term Impl' b) -> Term Impl' b
--   pmatchImpl (Impl impl) next = undefined 

-- class (PDSL edsl,
--        IsPTypeBackend
--          edsl
--          @Plutarch.PType.PPType
--          (a |> Sym (Plutarch.PType.D:R:PHs[0]))) =>
--       PConstructable' edsl a where
--   pconImpl :: GHC.Stack.Types.HasCallStack =>
--               PConcrete edsl a -> UnEDSLKind edsl a
--   pmatchImpl :: forall (b :: Plutarch.PType.PType).
--                 (GHC.Stack.Types.HasCallStack,
--                  IsPType
--                    edsl
--                    @Plutarch.PType.PPType
--                    (b |> Sym (Plutarch.PType.D:R:PHs[0]))) =>
--                 UnEDSLKind edsl a
--                 -> (PConcrete edsl a -> Term edsl b) -> Term edsl bu

-- | Lazy if-then-else.
-- pif :: PConstructable' edsl (PSOPed PBool) => Term edsl PBool -> Term edsl a -> Term edsl a -> Term edsl a
-- pif b case_true case_false = pmatch b $ \case
--   PTrue -> case_true
--   PFalse -> case_false

class    PLC a => PLC' a
instance PLC a => PLC' a

class    PSOP a => PSOP' a
instance PSOP a => PSOP' a

-- | Boolean negation for 'PBool' terms.
pnot :: PBoolCls edsl
     => Term edsl (PBool #-> PBool)
pnot = phoistAcyclic do plam \x -> pif' # x # pcon PFalse # pcon PTrue

-- | Lazily evaluated boolean and for 'PBool' terms.
infixr 3 #&&

(#&&) :: PBoolCls s
      => Term s PBool -> Term s PBool -> Term s PBool
x #&& y = pforce $ pand # x # pdelay y

-- | Lazily evaluated boolean or for 'PBool' terms.
infixr 2 #||

(#||) :: PBoolCls s
      => Term s PBool -> Term s PBool -> Term s PBool
x #|| y = pforce $ por # x # pdelay y

-- -- | Hoisted, Plutarch level, lazily evaluated boolean and function.
pand :: PBoolCls s => PIfThenElse s => PForce s => Term s (PBool #-> PDelayed PBool #-> PDelayed PBool)
pand = phoistAcyclic $ plam $ \x y -> pif' # x # y # (phoistAcyclic $ pdelay $ pcon PFalse)

-- | Hoisted, Plutarch level, strictly evaluated boolean and function.
pand' :: PBoolCls s
      => Term s (PBool #-> PBool #-> PBool)
pand' = phoistAcyclic $ plam $ \x y -> pif' # x # y # (pcon PFalse)

-- | Hoisted, Plutarch level, lazily evaluated boolean or function.
por :: PBoolCls s
     => Term s (PBool #-> PDelayed PBool #-> PDelayed PBool)
por = phoistAcyclic $ plam $ \x -> pif' # x # (phoistAcyclic $ pdelay $ pcon PTrue)

-- | Hoisted, Plutarch level, strictly evaluated boolean or function.
por' :: PBoolCls s
     => Term s (PBool #-> PBool #-> PBool)
por' = phoistAcyclic $ plam $ \x -> pif' # x # (pcon PTrue)

-- | Like Haskell's `and` but for Plutarch terms
pands :: PBoolCls s
      => [Term s PBool] -> Term s PBool
pands ts' =
  case nonEmpty ts' of
    Nothing -> pcon PTrue
    Just ts -> foldl1 (#&&) ts

-- -- | Generic version of (#==)
-- gpeq ::
--   forall t s.
--   ( PGeneric t
--   , PlutusType t
--   , All2 PEq (PCode t)
--   ) =>
--   Term s (t #-> t #-> PBool)
-- gpeq =
--   phoistAcyclic $
--     plam $ \x y ->
--       pmatch x $ \x' ->
--         pmatch y $ \y' ->
--           gpeq' (gpfrom x') (gpfrom y')

-- gpeq' :: All2 PEq xss => SOP (Term s) xss -> SOP (Term s) xss -> Term s PBool
-- gpeq' (SOP c1) (SOP c2) =
--   ccompare_NS (Proxy @(All PEq)) (pcon PFalse) eqProd (pcon PFalse) c1 c2

-- eqProd :: All PEq xs => NP (Term s) xs -> NP (Term s) xs -> Term s PBool
-- eqProd p1 p2 =
--   pands $ hcollapse $ hcliftA2 (Proxy :: Proxy PEq) eqTerm p1 p2
--   where
--     eqTerm :: forall s a. PEq a => Term s a -> Term s a -> K (Term s PBool) a
--     eqTerm a b =
--       K $ a #== b

