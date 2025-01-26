{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Repr.Data (
  PInnerIsData,
  PDataStruct (PDataStruct, unPDataStruct),
  PDataRec (PDataRec, unPDataRec),
  DeriveAsDataRec (DeriveAsDataRec, unDeriveAsDataRec),
  DeriveAsDataStruct (DeriveAsDataStruct, unDeriveAsDataStruct),
  PInnest,
) where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Exts (Any)
import GHC.TypeError (ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)
import Generics.SOP (
  All,
  All2,
  Code,
  K (K),
  NP (Nil, (:*)),
  NS (Z),
  SListI,
  SListI2,
  SOP (SOP),
 )
import Generics.SOP qualified as SOP
import Generics.SOP.Constraint (Head)
import Plutarch.Builtin.Data (
  PBuiltinList,
  PData,
  pasConstr,
  pconsBuiltin,
  pconstrBuiltin,
  pfstBuiltin,
  psndBuiltin,
 )
import Plutarch.Internal.Eq (PEq, (#==))
import Plutarch.Internal.IsData (PIsData)
import Plutarch.Internal.Lift (pconstant)
import Plutarch.Internal.ListLike (phead, ptail)
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  PContravariant',
  PContravariant'',
  PCovariant',
  PCovariant'',
  PInner,
  PVariant',
  PVariant'',
  PlutusType,
  pcon,
  pcon',
  pmatch,
  pmatch',
 )
import Plutarch.Internal.Term (S, Term, phoistAcyclic, plet, punsafeCoerce, (#), (#$))
import Plutarch.Repr.Internal (
  PRec (PRec, unPRec),
  PStruct (PStruct, unPStruct),
  RecTypePrettyError,
  StructSameRepr,
  UnTermRec,
  UnTermStruct,
  groupHandlers,
 )
import Plutarch.TermCont (pletC, unTermCont)

-- TODO: move this to Plutarch.Internal
type family PInnest' (a :: S -> Type) (b :: S -> Type) :: S -> Type where
  PInnest' a a = a
  PInnest' a _b = PInnest' (PInner a) a

type PInnest a = PInnest' (PInner a) a

type family PNormalIsData' a b :: Constraint where
  PNormalIsData' _ PData = ()
  PNormalIsData' a b =
    TypeError
      ( 'Text "Data representation can only hold types whose inner most representation is PData"
          ':$$: 'Text "Inner most representation of \""
            ':<>: 'ShowType a
            ':<>: 'Text "\" is \""
            ':<>: 'ShowType b
            ':<>: 'Text "\""
      )
      ~ ()

class (PNormalIsData' a (PInnest a), PInnest a ~ PData) => PNormalIsData a
instance (PNormalIsData' a (PInnest a), PInnest a ~ PData) => PNormalIsData a

-- TODO: refactor this
type family PInnerIsData' (a :: S -> Type) (b :: S -> Type) :: Constraint where
  PInnerIsData' PData _ = ()
  PInnerIsData' a a =
    TypeError
      ('ShowType a ':<>: 'Text " does not reduce into PData ")
      ~ ()
  PInnerIsData' a _b = PInnerIsData' (PInner a) a

class PInnerIsData' a (PInner a) => PInnerIsData a
instance PInnerIsData' a (PInner a) => PInnerIsData a

-- | @since WIP
newtype PDataStruct (struct :: [[S -> Type]]) (s :: S) = PDataStruct {unPDataStruct :: PStruct struct s}

-- | @since WIP
newtype PDataRec (struct :: [S -> Type]) (s :: S) = PDataRec {unPDataRec :: PRec struct s}

-- | @since WIP
instance (SListI2 struct, All2 PNormalIsData struct) => PlutusType (PDataStruct struct) where
  type PInner (PDataStruct struct) = PData
  type PCovariant' (PDataStruct struct) = All2 PCovariant'' struct
  type PContravariant' (PDataStruct struct) = All2 PContravariant'' struct
  type PVariant' (PDataStruct struct) = All2 PVariant'' struct
  pcon' (PDataStruct x) = punsafeCoerce $ pconDataStruct x
  pmatch' x f = pmatchDataStruct (punsafeCoerce x) (f . PDataStruct)

-- | @since WIP
instance (SListI struct, All PNormalIsData struct) => PlutusType (PDataRec struct) where
  type PInner (PDataRec struct) = PBuiltinList PData
  type PCovariant' (PDataRec struct) = All PCovariant'' struct
  type PContravariant' (PDataRec struct) = All PContravariant'' struct
  type PVariant' (PDataRec struct) = All PVariant'' struct
  pcon' (PDataRec x) = punsafeCoerce $ pconDataRec x
  pmatch' x f = pmatchDataRec (punsafeCoerce x) (f . PDataRec)

-- | @since WIP
instance PEq (PDataStruct struct) where
  a #== b = pto a #== pto b

-- | @since WIP
instance PEq (PDataRec struct) where
  a #== b = pto a #== pto b

-- | @since WIP
instance PIsData (PDataRec struct)

-- | @since WIP
instance PIsData (PDataStruct struct)

-- | @since WIP
newtype DeriveAsDataRec (a :: S -> Type) s = DeriveAsDataRec {unDeriveAsDataRec :: a s}

-- | @since WIP
instance
  forall (a :: S -> Type) (struct' :: [Type]) (struct :: [S -> Type]).
  ( SOP.Generic (a Any)
  , '[struct'] ~ Code (a Any)
  , struct ~ UnTermRec struct'
  , All PNormalIsData struct
  , SListI struct
  , forall s. StructSameRepr s a '[struct]
  , RecTypePrettyError (Code (a Any))
  ) =>
  PlutusType (DeriveAsDataRec a)
  where
  type PInner (DeriveAsDataRec a) = PDataRec (UnTermRec (Head (Code (a Any))))
  type PCovariant' (DeriveAsDataRec a) = PCovariant' a
  type PContravariant' (DeriveAsDataRec a) = PContravariant' a
  type PVariant' (DeriveAsDataRec a) = PVariant' a
  pcon' (DeriveAsDataRec x) =
    pcon $ PDataRec $ PRec $ SOP.unZ $ SOP.unSOP $ SOP.hcoerce $ SOP.from x
  pmatch' x f =
    pmatch x (f . DeriveAsDataRec . SOP.to . SOP.hcoerce . SOP . (Z @_ @_ @'[]) . unPRec . unPDataRec)

-- | @since WIP
newtype DeriveAsDataStruct (a :: S -> Type) s = DeriveAsDataStruct {unDeriveAsDataStruct :: a s}

-- | @since WIP
instance
  forall (a :: S -> Type) (struct :: [[S -> Type]]).
  ( SOP.Generic (a Any)
  , struct ~ UnTermStruct (a Any)
  , All2 PNormalIsData struct
  , SListI2 struct
  , forall s. StructSameRepr s a struct
  ) =>
  PlutusType (DeriveAsDataStruct a)
  where
  type PInner (DeriveAsDataStruct a) = PDataStruct (UnTermStruct (a Any))
  type PCovariant' (DeriveAsDataStruct a) = PCovariant' a
  type PContravariant' (DeriveAsDataStruct a) = PContravariant' a
  type PVariant' (DeriveAsDataStruct a) = PVariant' a
  pcon' (DeriveAsDataStruct x) =
    pcon @(PDataStruct (UnTermStruct (a Any))) $ PDataStruct $ PStruct $ SOP.hcoerce $ SOP.from x
  pmatch' x f =
    pmatch @(PDataStruct (UnTermStruct (a Any))) x (f . DeriveAsDataStruct . SOP.to . SOP.hcoerce . unPStruct . unPDataStruct)

-- Helpers

-- NOTE: I'm intentionally not providing default ordering instance here because ordering of
-- product and sum types can be handled in various ways:
-- https://en.wikipedia.org/wiki/Total_order#Orders_on_the_Cartesian_product_of_totally_ordered_sets
-- If anyone can make convincing argument to support one over another, it can be done easily
-- instance (PlutusType (PDataRec struct), All PPartialOrd struct) => PPartialOrd (PDataRec struct) where
--   x #<= y = undefined
--   a #< b = undefined

pconDataRec ::
  forall (struct :: [S -> Type]) (s :: S).
  All PNormalIsData struct =>
  PRec struct s ->
  Term s (PDataRec struct)
pconDataRec (PRec xs) =
  let
    collapesdData :: [Term s PData]
    collapesdData = SOP.hcollapse $ SOP.hcmap (Proxy @PNormalIsData) (K . punsafeCoerce) xs
    builtinList = foldr (\x xs -> pconsBuiltin # x # xs) (pconstant []) collapesdData
   in
    punsafeCoerce builtinList

pconDataStruct ::
  forall (struct :: [[S -> Type]]) (s :: S).
  (SListI2 struct, All2 PNormalIsData struct) =>
  PStruct struct s ->
  Term s (PDataStruct struct)
pconDataStruct (PStruct xs) =
  let
    collapesdData = SOP.hcollapse $ SOP.hcmap (Proxy @PNormalIsData) (K . punsafeCoerce) xs
    builtinList = foldr (\x xs -> pconsBuiltin # x # xs) (pconstant []) collapesdData
    idx = pconstant $ toInteger $ SOP.hindex xs
   in
    punsafeCoerce $ pconstrBuiltin # idx #$ builtinList

newtype H s struct = H
  { unH ::
      forall r.
      (forall s'. Term s' (PBuiltinList PData) -> Term s' PData) ->
      Term s (PBuiltinList PData) ->
      (PRec struct s -> Term s r) ->
      Term s r
  }

pmatchDataRec ::
  forall (struct :: [S -> Type]) b s.
  All PNormalIsData struct =>
  Term s (PDataRec struct) ->
  (PRec struct s -> Term s b) ->
  Term s b
pmatchDataRec (punsafeCoerce -> x) f =
  let
    go :: forall y ys. H s ys -> H s (y ': ys)
    go (H rest) = H $ \getCurr ds cps ->
      let
        getNext :: (forall s'. Term s' (PBuiltinList PData) -> Term s' PData)
        getNext xs = getCurr $ ptail # xs -- undefined -- phoistAcyclic $ plam $ \d -> getCurr #$ ptail # d
        getCurrHoisted = phoistAcyclic $ plam getCurr
        parsed = punsafeCoerce $ getCurrHoisted # ds
       in
        rest getNext ds $ \(PRec rest') ->
          cps $ PRec $ parsed :* rest'

    record :: Term s (PBuiltinList PData) -> (PRec struct s -> Term s r) -> Term s r
    record = (unH $ SOP.para_SList (H $ \_ _ cps -> cps $ PRec Nil) go) (phead #)
   in
    plet x (`record` f)

newtype StructureHandler s r struct = StructureHandler
  { unSBR ::
      Integer ->
      Term s (PBuiltinList PData) ->
      (PStruct struct s -> Term s r) ->
      NP (K (Integer, Term s r)) struct
  }

-- This is probably general enough to be used for non-Data encoded types
pmatchDataStruct ::
  forall (struct :: [[S -> Type]]) b s.
  All2 PNormalIsData struct =>
  Term s (PDataStruct struct) ->
  (PStruct struct s -> Term s b) ->
  Term s b
pmatchDataStruct (punsafeCoerce -> x) f = unTermCont $ do
  let
    pgetConstrBody = phoistAcyclic $ plam $ \d -> psndBuiltin #$ pasConstr # d
    go :: forall y ys. All PNormalIsData y => StructureHandler s b ys -> StructureHandler s b (y ': ys)
    go (StructureHandler rest) = StructureHandler $ \i ds cps ->
      let
        dataRecAsBuiltinList :: Term s (PBuiltinList PData) -> Term s (PDataRec y)
        dataRecAsBuiltinList = punsafeCoerce

        handler = pmatchDataRec @y (dataRecAsBuiltinList ds) $ \(PRec r) -> cps $ PStruct $ SOP $ Z r
        restHandlers = rest (i + 1) ds (\(PStruct (SOP sop)) -> cps $ PStruct $ SOP $ SOP.S sop)
       in
        K (i, handler) :* restHandlers

    -- This builds "handlers"--that is each cases of SOP data
    -- By building this we can figure out which cases share same computation, hence which branches to group
    handlers' :: StructureHandler s b struct
    handlers' = SOP.cpara_SList (Proxy @(All PNormalIsData)) (StructureHandler $ \_ _ _ -> Nil) go

    handlers :: Term s (PBuiltinList PData) -> [(Integer, Term s b)]
    handlers d = SOP.hcollapse $ unSBR handlers' 0 d f

  case handlers (pgetConstrBody # x) of
    [(_, h)] -> pure h
    _ -> do
      x' <- pletC $ pasConstr # x
      idx <- pletC $ pfstBuiltin # x'
      ds <- pletC $ psndBuiltin # x'

      pure $ groupHandlers (handlers ds) idx
