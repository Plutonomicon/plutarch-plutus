{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

{- | Administrative normal form representation, with alpha-equivalent binds
coalesced.

= Links

- [Description of administrative normal
form](https://en.wikipedia.org/wiki/A-normal_form)

@since wip
-}
module Plutarch.Backend.ANF (
  Leaf (..),
  Ref (..),
  ANFBind (..),
  Id (..),
  ANF (..),
  Demand (..),
  fromHashedAST,
  analyzeDemand,
  getANFBindAnn,
) where

import Control.Monad.ST (ST, runST)
import Control.Monad.State.Strict (
  State,
  gets,
  modify,
  runState,
 )
import Data.Bifunctor (bimap)
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.Foldable (for_, traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.Vector (MVector, Vector)
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as MVector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Data.Word (Word64)
import Plutarch.Backend.AST (
  AST (
    ASTApply,
    ASTCase,
    ASTCompose,
    ASTConstr,
    ASTDelay,
    ASTFix,
    ASTForce,
    ASTLam,
    ASTLeaf
  ),
  Hash,
  Multiplicity,
 )
import Plutarch.Backend.AST qualified as AST
import Plutarch.Backend.UPLC (UPLCTerm)
import PlutusCore (Some (Some), ValueOf (ValueOf))
import PlutusCore qualified as PLC

{- | A leaf bind in the ANF (that is, one that cannot have dependencies).

@since wip
-}
data Leaf (ann :: Type)
  = LConstant ann (Some (ValueOf PLC.DefaultUni))
  | LBuiltin ann PLC.DefaultFun
  | LCompiled ann UPLCTerm
  | LError ann
  deriving stock
    ( -- | @since wip
      Functor
    , -- | @since wip
      Show
    , -- | @since wip
      Eq
    )

{- | As ANF \'inlines\' variables, subcomputations are either variables
(identified by their hashes) or other binds (identified by their
identifiers).

@since wip
-}
data Ref
  = AVar Hash
  | AnId Id
  deriving stock
    ( -- | @since wip
      Show
    , -- | @since wip
      Eq
    )

{- | An identifier for an ANF bind.

@since wip
-}
newtype Id = Id Int
  deriving
    ( -- | @since wip
      Eq
    , -- | @since wip
      Ord
    )
    via Int
  deriving stock
    ( -- | @since wip
      Show
    )

{- | An ANF bind. Instead of a recursive ANF-like structure, this uses 'Id' to
determine subcomputations.

@since wip
-}
data ANFBind (ann :: Type)
  = ANFLeaf (Leaf ann)
  | ANFForce ann Ref
  | ANFDelay ann Ref
  | ANFLam ann (NonEmptyVector (Maybe Multiplicity)) Ref
  | ANFFix ann Multiplicity Ref
  | ANFApply ann Ref (NonEmptyVector Ref)
  | ANFConstr ann Word64 (Vector Ref)
  | ANFCase ann Ref (NonEmptyVector Ref)
  | ANFCompose ann (NonEmptyVector Ref)
  deriving stock
    ( -- | @since wip
      Show
    , -- | @since wip
      Functor
    , -- | @since wip
      Eq
    )

-- | @since wip
getANFBindAnn :: forall (ann :: Type). ANFBind ann -> ann
getANFBindAnn = \case
  ANFLeaf ell -> case ell of
    LConstant x _ -> x
    LBuiltin x _ -> x
    LCompiled x _ -> x
    LError x -> x
  ANFForce x _ -> x
  ANFDelay x _ -> x
  ANFLam x _ _ -> x
  ANFFix x _ _ -> x
  ANFApply x _ _ -> x
  ANFConstr x _ _ -> x
  ANFCase x _ _ -> x
  ANFCompose x _ -> x

{- | A combination of a (nonempty) vector of binds, together with a unique
mapping between identifiers and hashes of unique subcomputations.

@since wip
-}
data ANF (ann :: Type) = ANF (Bimap Id Hash) (NonEmptyVector (ANFBind ann))

{- | Given an 'AST' annotated with hashes for unique (up to alpha-equivalence)
subcomputations, construct an ANF. The 'Hash' annotations are used to
identify alpha-equivalent subcomputations, so that they correspond to a
single unique bind.

@since wip
-}
fromHashedAST :: AST Hash -> ANF ()
fromHashedAST ast = case runState (go ast) (Bimap.empty, IntMap.empty) of
  -- Note (Koz, 05/06/2026): Due to how we construct the IntMap here, there is
  -- no chance of 'missing' or non-contiguous keys. Thus, the `lookup` cannot
  -- fail.
  (_, (bm, im)) -> ANF bm . NEVector.generate1 (IntMap.size im) $ \i -> fromJust . IntMap.lookup i $ im
  where
    -- We maintain the partially-constructed bind list as an `IntMap` mostly for
    -- efficiency, as incrementally expanding a `Vector` is quite tedious and
    -- inefficient.
    go :: AST Hash -> State (Bimap Id Hash, IntMap (ANFBind ())) Ref
    go = \case
      ASTLeaf ell -> doLeaf ell
      ASTForce h body -> withLookup h $ do
        bodyRef <- go body
        newBind h (ANFForce () bodyRef)
      ASTDelay h body -> withLookup h $ do
        bodyRef <- go body
        newBind h (ANFDelay () bodyRef)
      ASTLam h mults body -> withLookup h $ do
        bodyRef <- go body
        newBind h (ANFLam () mults bodyRef)
      ASTFix h mult body -> withLookup h $ do
        bodyRef <- go body
        newBind h (ANFFix () mult bodyRef)
      ASTApply h f xs -> withLookup h $ do
        fRef <- go f
        xsRefs <- traverse go xs
        newBind h (ANFApply () fRef xsRefs)
      ASTConstr h tag fields -> withLookup h $ do
        fieldsRefs <- traverse go fields
        newBind h (ANFConstr () tag fieldsRefs)
      ASTCase h scrut handlers -> withLookup h $ do
        scrutRef <- go scrut
        handlersRefs <- traverse go handlers
        newBind h (ANFCase () scrutRef handlersRefs)
      ASTCompose h components -> withLookup h $ do
        componentsRefs <- traverse go components
        newBind h (ANFCompose () componentsRefs)
    doLeaf :: AST.Leaf Hash -> State (Bimap Id Hash, IntMap (ANFBind ())) Ref
    doLeaf = \case
      AST.LVar _ h -> pure . AVar $ h
      AST.LConstant h c -> withLookup h $ newBind h (ANFLeaf (LConstant () c))
      AST.LBuiltin h f -> withLookup h $ newBind h (ANFLeaf (LBuiltin () f))
      AST.LCompiled h code -> withLookup h $ newBind h (ANFLeaf (LCompiled () code))
      AST.LError h -> withLookup h $ newBind h (ANFLeaf (LError ()))
    -- Checks if we've already seen an alpha-equivalent bind. If so, yield a
    -- `Ref` to it, otherwise take the supplied action to build it, and produce
    -- the `Ref` from that.
    withLookup ::
      Hash ->
      State (Bimap Id Hash, IntMap (ANFBind ())) Ref ->
      State (Bimap Id Hash, IntMap (ANFBind ())) Ref
    withLookup h act = do
      mId <- gets (Bimap.lookupR h . fst)
      maybe act (pure . AnId) mId
    newBind :: Hash -> ANFBind () -> State (Bimap Id Hash, IntMap (ANFBind ())) Ref
    newBind h bind = do
      -- Ensures we have contiguous keys starting from 0.
      firstAvailable <- gets (maybe 0 ((+ 1) . fst) . IntMap.lookupMax . snd)
      let asId = Id firstAvailable
      modify (bimap (Bimap.insert asId h) (IntMap.insert firstAvailable bind))
      pure . AnId $ asId

{- | A custom monoid for demand analysis. This is designed to fuse both
determining how often a bind is needed, and where it should be @let@-bound,
into a single monoidal traversal.

@since wip
-}
data Demand
  = {- | The 'mempty' starting point.

    @since wip
    -}
    NeverDemanded
  | {- | The 'Id' corresponds to the last bind (thus, the first demand site) of
    the given bind, and the 'Word64' is the count of how many times the bind
    is demanded.

    @since wip
    -}
    Demanded Id Word64
  | {- | Some things should never be @let@-bound. This means their demand
    analysis is trivial.

    @since wip
    -}
    Trivial
  deriving stock
    ( -- | @since wip
      Eq
    , -- | @since wip
      Show
    )

-- | @since wip
instance Semigroup Demand where
  NeverDemanded <> x = x
  x <> NeverDemanded = x
  Demanded (Id i) count1 <> Demanded (Id j) count2 =
    Demanded (Id $ max i j) (count1 + count2)
  Trivial <> _ = Trivial
  _ <> Trivial = Trivial

-- | @since wip
instance Monoid Demand where
  mempty = NeverDemanded

analyzeDemand :: forall (ann :: Type). ANF ann -> ANF Demand
analyzeDemand (ANF bm binds) = runST $ do
  let len = NEVector.length binds
  -- Note (Koz, 05/06/2026): We're working with a possibly-empty mutable vector
  -- here as currently, there is no way to 'freeze' a mutable non-empty vector.
  mv <- MVector.new len
  for_ [0, 1 .. len - 1] $ \i -> case binds NEVector.! i of
    ANFLeaf ell -> MVector.write mv i . ANFLeaf $ case ell of
      LConstant _ c ->
        if smallEnoughToInline c
          then LConstant Trivial c
          else LConstant mempty c
      LBuiltin _ f -> LBuiltin Trivial f
      LCompiled _ code -> LCompiled mempty code
      LError _ -> LError Trivial
    ANFForce _ r -> do
      updateDemandAt mv i r
      MVector.write mv i . ANFForce mempty $ r
    ANFDelay _ r -> do
      updateDemandAt mv i r
      MVector.write mv i . ANFDelay mempty $ r
    ANFLam _ mults r -> do
      updateDemandAt mv i r
      MVector.write mv i . ANFLam mempty mults $ r
    ANFFix _ mult r -> do
      updateDemandAt mv i r
      MVector.write mv i . ANFFix mempty mult $ r
    ANFApply _ f xs -> do
      updateDemandAt mv i f
      traverse_ (updateDemandAt mv i) xs
      MVector.write mv i . ANFApply mempty f $ xs
    ANFConstr _ tag fields -> do
      traverse_ (updateDemandAt mv i) fields
      MVector.write mv i . ANFConstr mempty tag $ fields
    ANFCase _ scrut handlers -> do
      updateDemandAt mv i scrut
      traverse_ (updateDemandAt mv i) handlers
      MVector.write mv i . ANFCase mempty scrut $ handlers
    ANFCompose _ components -> do
      traverse_ (updateDemandAt mv i) components
      MVector.write mv i . ANFCompose mempty $ components
  v <- Vector.unsafeFreeze mv
  pure . ANF bm . NEVector.unsafeFromVector $ v
  where
    smallEnoughToInline :: Some (ValueOf PLC.DefaultUni) -> Bool
    smallEnoughToInline = \case
      Some (ValueOf PLC.DefaultUniBool _) -> True
      Some (ValueOf PLC.DefaultUniUnit _) -> True
      Some (ValueOf PLC.DefaultUniInteger n) -> abs n < 256
      _ -> False
    updateDemandAt ::
      forall (s :: Type).
      MVector s (ANFBind Demand) ->
      Int ->
      Ref ->
      ST s ()
    updateDemandAt mv i = \case
      AnId (Id j) -> MVector.modify mv (fmap (<> Demanded (Id i) 1)) j
      AVar _ -> pure ()
