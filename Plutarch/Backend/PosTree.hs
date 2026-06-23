{- | Position trees, as described by the /Hashing Modulo Alpha-Equivalence/
paper. These are used to indicate which node in any given AST corresponds to
a specific bound or free variable.

Intuitively, these can be thought of as (possibly branching) paths through an
AST, always ending at a leaf node corresponding to a variable.

= Links

- [The original paper](https://arxiv.org/pdf/2105.02856)

@since wip
-}
module Plutarch.Backend.PosTree (
  PosTree (..),
  isLinear,
) where

import Control.Monad (guard)
import Data.Foldable (sequenceA_)
import Data.Hashable (
  Hashable (hash, hashWithSalt),
  defaultHashWithSalt,
 )
import Data.Maybe (isJust, isNothing)
import Data.These (These (That, These, This))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector

{- | A position tree.

@since wip
-}
data PosTree
  = {- | Marks the variable of interest.

    @since wip
    -}
    PHere
  | {- | The variable of interest is in the tree of our direct descendant.
    @since wip
    -}
    POne PosTree
  | {- | The variable of interest is in one, or both, of our subtrees.

    @since wip
    -}
    PTwo (These PosTree PosTree)
  | {- | The variable of interest is in at least one of our subtrees. A
    higher-than-2 variant of 'PTwo'.

    = Note

    This structure must maintain the implicit invariant that at least one of
    the entries of the 'Vector' is not 'Nothing'. All operations we provide
    over 'PosTree' maintain this invariant, but if you build these yourself,
    ensure you also maintain it.

    @since wip
    -}
    PMany (Vector (Maybe PosTree))
  | {- | The variable of interest occurs in a @case@,
    in either the scrutinee subtree, one or more of the handler subtrees, or
    both.

    = Note

    This structure must maintain the implicit invariant that at least one of
    the following always holds:

    * The scrutinee 'PosTree' is not 'Nothing'; or
    * At least one of the 'NonEmptyVector' entries is not 'Nothing'.

    All operations we provide over 'PosTree' maintain this invariant, but if
    you build these yourself, ensure you also maintain it.

    @since wip
    -}
    PCase (Maybe PosTree) (NonEmptyVector (Maybe PosTree))
  | {- | The variable of interest occurs in a composition.

    = Note

    This structure must maintain the following implicit invariants:

    * The 'NonEmptyVector' has at least two elements; and
    * At least one of the 'NonEmptyVector' entries is not 'Nothing'.

    All operations we provide over 'PosTree' maintain this invariant, but if
    you build these yourself, ensure you also maintain these.

    @since wip
    -}
    PCompose (NonEmptyVector (Maybe PosTree))
  deriving stock
    ( -- | @since wip
      Show
    , -- | @since wip
      Eq
    )

-- | @since wip
instance Hashable PosTree where
  {-# INLINEABLE hashWithSalt #-}
  hashWithSalt = defaultHashWithSalt
  {-# INLINEABLE hash #-}
  hash = \case
    PHere -> hash (0 :: Int)
    POne t -> hash (1 :: Int, t)
    PTwo ts -> hash (2 :: Int, ts)
    PMany ts -> hash (3 :: Int, Vector.toList ts)
    PCase t ts -> hash (4 :: Int, t, NEVector.toList ts)
    PCompose ts -> hash (5 :: Int, NEVector.toList ts)

{- | Checks if a 'PosTree' corresponds to a non-branching path. In the context
of variables, checks whether the variable's use is linear (no more than
once).

@since wip
-}
isLinear :: PosTree -> Bool
isLinear = \case
  PHere -> True
  POne t -> isLinear t
  PTwo these -> case these of
    This t -> isLinear t
    That t -> isLinear t
    These _ _ -> False
  PMany ts -> case Vector.uncons ts of
    Nothing -> True
    Just (x, xs) -> case x of
      Nothing -> maybe False isLinear (findOnlyJust xs)
      Just t -> isLinear t && Vector.all isNothing xs
  PCase t ts -> case fmap isLinear t of
    Nothing -> case NEVector.uncons ts of
      (x, xs) -> case x of
        Nothing -> maybe False isLinear (findOnlyJust xs)
        Just t -> isLinear t && Vector.all isNothing xs
    Just tLinearity -> case sequenceA_ ts of
      Nothing -> tLinearity
      Just _ -> False
  PCompose ts -> case NEVector.uncons ts of
    (x, xs) -> case x of
      Nothing -> maybe False isLinear (findOnlyJust xs)
      Just t -> isLinear t && Vector.all isNothing xs
  where
    -- Note (Koz, 23/06/2026): This works because we assume we never pass any
    -- collection to this function that could be `Nothing` everywhere.
    findOnlyJust :: Vector (Maybe PosTree) -> Maybe PosTree
    findOnlyJust v = do
      (i, is) <- Vector.uncons . Vector.findIndices isJust $ v
      guard (Vector.null is)
      v Vector.! i
