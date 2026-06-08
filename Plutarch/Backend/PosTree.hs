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

import Data.Foldable (sequenceA_)
import Data.Hashable (
  Hashable (hash, hashWithSalt),
  defaultHashWithSalt,
 )
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
  | {- | The variable of interest occurs in an application or @case@,
    in either the function (scrutinee) subtree, one or more of the argument
    (handler) subtrees, or both.

    = Note

    This structure must maintain the implicit invariant that at least one of
    the following always holds:

    - The function (scrutinee) 'PosTree' is not 'Nothing'; or
    - At least one of the 'NonEmptyVector' entries is not 'Nothing'.

    All operations we provide over 'PosTree' maintain this invariant, but if
    you build these yourself, ensure you also maintain it.

    @since wip
    -}
    PApplyCase (Maybe PosTree) (NonEmptyVector (Maybe PosTree))
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
    PApplyCase t ts -> hash (4 :: Int, t, NEVector.toList ts)

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
    Just (x, xs) -> case Vector.foldl' go (fmap isLinear x) xs of
      Nothing -> False -- impossible
      Just linearity -> linearity
  PApplyCase t ts -> case fmap isLinear t of
    Nothing -> case NEVector.uncons ts of
      (x, xs) -> case Vector.foldl' go (fmap isLinear x) xs of
        Nothing -> False -- impossible
        Just linearity -> linearity
    Just tLinearity -> case sequenceA_ ts of
      Nothing -> tLinearity
      Just _ -> False
  where
    go :: Maybe Bool -> Maybe PosTree -> Maybe Bool
    go acc x = case acc of
      Nothing -> fmap isLinear x
      Just False -> Just False
      Just True -> case fmap isLinear x of
        Nothing -> Just True
        Just _ -> Just False
