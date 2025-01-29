{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Repr.Data (
  PInnerMostIsData,
  PDataStruct (PDataStruct, unPDataStruct),
  PDataRec (PDataRec, unPDataRec),
  DeriveAsDataRec (DeriveAsDataRec, unDeriveAsDataRec),
  DeriveAsDataStruct (DeriveAsDataStruct, unDeriveAsDataStruct),
  PInnerMost,
) where

import Data.Kind (Constraint, Type)
import Data.Maybe (catMaybes)
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
import Plutarch.Internal.Term (S, Term, phoistAcyclic, plet, pplaceholder, punsafeCoerce, (#), (#$))
import Plutarch.Repr.Internal (
  PRec (PRec, unPRec),
  PStruct (PStruct, unPStruct),
  RecTypePrettyError,
  StructSameRepr,
  UnTermRec,
  UnTermStruct,
  groupHandlers,
 )
import Plutarch.TermCont (pfindPlaceholder, pletC, unTermCont)

-- TODO: move this to Plutarch.Internal
type family PInnerMost' (a :: S -> Type) (b :: S -> Type) :: S -> Type where
  PInnerMost' a a = a
  PInnerMost' a _b = PInnerMost' (PInner a) a

type PInnerMost a = PInnerMost' (PInner a) a

type family PInnerMostIsData' a b :: Constraint where
  PInnerMostIsData' _ PData = ()
  PInnerMostIsData' a b =
    TypeError
      ( 'Text "Data representation can only hold types whose inner most representation is PData"
          ':$$: 'Text "Inner most representation of \""
            ':<>: 'ShowType a
            ':<>: 'Text "\" is \""
            ':<>: 'ShowType b
            ':<>: 'Text "\""
      )
      ~ ()

class (PInnerMostIsData' a (PInnerMost a), PInnerMost a ~ PData) => PInnerMostIsData a
instance (PInnerMostIsData' a (PInnerMost a), PInnerMost a ~ PData) => PInnerMostIsData a

-- | @since WIP
newtype PDataStruct (struct :: [[S -> Type]]) (s :: S) = PDataStruct {unPDataStruct :: PStruct struct s}

-- | @since WIP
newtype PDataRec (struct :: [S -> Type]) (s :: S) = PDataRec {unPDataRec :: PRec struct s}

-- | @since WIP
instance (SListI2 struct, All2 PInnerMostIsData struct) => PlutusType (PDataStruct struct) where
  type PInner (PDataStruct struct) = PData
  type PCovariant' (PDataStruct struct) = All2 PCovariant'' struct
  type PContravariant' (PDataStruct struct) = All2 PContravariant'' struct
  type PVariant' (PDataStruct struct) = All2 PVariant'' struct
  pcon' (PDataStruct x) = punsafeCoerce $ pconDataStruct x
  pmatch' x f = pmatchDataStruct (punsafeCoerce x) (f . PDataStruct)

-- | @since WIP
instance (SListI struct, All PInnerMostIsData struct) => PlutusType (PDataRec struct) where
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
  , All PInnerMostIsData struct
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
  , All2 PInnerMostIsData struct
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
  All PInnerMostIsData struct =>
  PRec struct s ->
  Term s (PDataRec struct)
pconDataRec (PRec xs) =
  let
    collapesdData :: [Term s PData]
    collapesdData = SOP.hcollapse $ SOP.hcmap (Proxy @PInnerMostIsData) (K . punsafeCoerce) xs
    builtinList = foldr (\x xs -> pconsBuiltin # x # xs) (pconstant []) collapesdData
   in
    punsafeCoerce builtinList

pconDataStruct ::
  forall (struct :: [[S -> Type]]) (s :: S).
  (SListI2 struct, All2 PInnerMostIsData struct) =>
  PStruct struct s ->
  Term s (PDataStruct struct)
pconDataStruct (PStruct xs) =
  let
    collapesdData = SOP.hcollapse $ SOP.hcmap (Proxy @PInnerMostIsData) (K . punsafeCoerce) xs
    builtinList = foldr (\x xs -> pconsBuiltin # x # xs) (pconstant []) collapesdData
    idx = pconstant $ toInteger $ SOP.hindex xs
   in
    punsafeCoerce $ pconstrBuiltin # idx #$ builtinList

newtype PHB s struct struct' = PHB
  { unPHB ::
      Integer ->
      (PRec struct' s -> PRec struct s) ->
      (PRec struct s, Integer)
  }

newtype H s struct = H
  { unH ::
      forall r.
      Integer ->
      Term s (PBuiltinList PData) ->
      (Integer, Term s (PBuiltinList PData)) ->
      (PRec struct s -> Term s r) ->
      Term s r
  }

pmatchDataRec ::
  forall (struct :: [S -> Type]) b s.
  All PInnerMostIsData struct =>
  Term s (PDataRec struct) ->
  (PRec struct s -> Term s b) ->
  Term s b
pmatchDataRec (punsafeCoerce -> x) f = unTermCont $ do
  let
    placeholderBuilder :: forall y ys. PHB s struct ys -> PHB s struct (y ': ys)
    placeholderBuilder (PHB rest) = PHB $ \idx g ->
      rest (idx + 1) (\(PRec prev) -> g (PRec $ pplaceholder idx :* prev))

    placeholder :: (PRec struct s, Integer)
    placeholder = (unPHB $ SOP.para_SList (PHB $ \idx g -> (g (PRec Nil), idx - 1)) placeholderBuilder) 0 id

  usedFields <-
    catMaybes
      <$> traverse
        ( \idx -> do
            found <- pfindPlaceholder idx (f $ fst placeholder)
            pure $ if found then Just idx else Nothing
        )
        ([0 .. (snd placeholder)] :: [Integer])

  let
    -- Technically, we don't need @running@ here since we are pretty sure there's nothing using field
    -- that is not found in @usedFields@--because one can only @evalTerm@ closed terms and using bound field
    -- means free variable. I added it regardlessly.
    --
    -- Also, this can be easily tweaked to have different performance characteristics
    -- (i.e. CPU/MEM optimized or Size optimized).
    -- Currently, to balance off both, we hoist "dropToCurrent" function. However, it could be made to
    -- cost even less on CPU/MEM by not hoisting it. It is likely not a good trade off if there's a
    -- whole bunch of record deconstruction.
    go :: forall y ys. H s ys -> H s (y ': ys)
    go (H rest) =
      H $ \idx running lastBind@(lastBindIdx, lastBindT) g ->
        if idx `elem` usedFields
          then
            let
              -- See if this is last field being used.
              lastBind = all (<= idx) usedFields
              dropAmount = fromInteger $ idx - lastBindIdx
              dropToCurrent' :: forall s'. Term s' (PBuiltinList PData) -> Term s' (PBuiltinList PData)
              dropToCurrent' = foldr (.) id $ replicate dropAmount (ptail #)

              -- If this is last term, or amount of @ptail@ we need is less than 3, we don't hoist.
              currentTerm
                | lastBind || dropAmount <= 3 = dropToCurrent' lastBindT
                | otherwise = phoistAcyclic (plam dropToCurrent') # lastBindT
             in
              -- If this is the last field, we don't need to plet.
              (if lastBind then (\a h -> h a) else plet) currentTerm $ \newBind ->
                rest
                  (idx + 1)
                  (ptail # running)
                  (idx, newBind)
                  $ \(PRec rest') -> g $ PRec $ punsafeCoerce (phead # newBind) :* rest'
          else rest
            (idx + 1)
            (ptail # running)
            lastBind
            $ \(PRec rest') -> g $ PRec $ punsafeCoerce (phead # running) :* rest'

    record :: Term s (PBuiltinList PData) -> (PRec struct s -> Term s r) -> Term s r
    record ds = (unH $ SOP.para_SList (H $ \_ _ _ g -> g $ PRec Nil) go) 0 ds (0, ds)

  pure $ record x f

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
  All2 PInnerMostIsData struct =>
  Term s (PDataStruct struct) ->
  (PStruct struct s -> Term s b) ->
  Term s b
pmatchDataStruct (punsafeCoerce -> x) f = unTermCont $ do
  let
    go :: forall y ys. All PInnerMostIsData y => StructureHandler s b ys -> StructureHandler s b (y ': ys)
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
    handlers' = SOP.cpara_SList (Proxy @(All PInnerMostIsData)) (StructureHandler $ \_ _ _ -> Nil) go

    handlers :: Term s (PBuiltinList PData) -> [(Integer, Term s b)]
    handlers d = SOP.hcollapse $ unSBR handlers' 0 d f

  case handlers (psndBuiltin #$ pasConstr # x) of
    [(_, h)] -> pure h
    _ -> do
      x' <- pletC $ pasConstr # x
      idx <- pletC $ pfstBuiltin # x'
      ds <- pletC $ psndBuiltin # x'

      pure $ groupHandlers (handlers ds) idx
