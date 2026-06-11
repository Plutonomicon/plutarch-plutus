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
  fromHashedAST,
) where

import Control.Monad.State.Strict (
  State,
  gets,
  modify,
  runState,
 )
import Data.Bifunctor (bimap)
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Data.Word (Word64)
import Plutarch.Backend.AST (
  AST (
    ASTApply,
    ASTCase,
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
import PlutusCore (Some, ValueOf)
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
  deriving stock
    ( -- | @since wip
      Show
    )

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
