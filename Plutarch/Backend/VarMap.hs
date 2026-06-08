{-# LANGUAGE NoPartialTypeSignatures #-}

{- | A variable map, as described by the /Hashing Modulo Alpha-Equivalence/
paper. This represents all the free variables in a given (sub)-expression,
along with 'PosTrees' indicating where these free variables occur.

This is essentially @'Map' 'Word64' 'PosTree'@, but closed as a type to allow
us to provide higher-level operations more concisely.

= Links

- [The original paper](https://arxiv.org/pdf/2105.02856)

@since wip
-}
module Plutarch.Backend.VarMap (
  VarMap,
  vmEmpty,
  vmSingleton,
  vmDelete,
  vmMap,
  vmMerge,
  vmMergeM,
  vmExtend,
  vmFold,
) where

import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Map.Merge.Strict (WhenMatched, mergeA, preserveMissing)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Plutarch.Backend.PosTree (PosTree)

-- A variable map.
--
-- @since wip
newtype VarMap = VarMap (Map Word64 PosTree)
  deriving
    ( -- | @since wip
      Eq
    , -- | @since wip
      Hashable
    )
    via (Map Word64 PosTree)
  deriving stock
    ( -- | @since wip
      Show
    )

{- | An empty variable map.

@since wip
-}
vmEmpty :: VarMap
vmEmpty = VarMap Map.empty

{- | Produce a variable map mapping a single unique identifier to a position
tree.

@since wip
-}
vmSingleton :: Word64 -> PosTree -> VarMap
vmSingleton k = VarMap . Map.singleton k

{- | Given a variable identifier, remove it from the variable map if it exists,
and return it, along with whatever is left in the variable map.

@since wip
-}
vmDelete :: Word64 -> VarMap -> (Maybe PosTree, VarMap)
vmDelete k (VarMap m) = (Map.lookup k m, VarMap . Map.delete k $ m)

{- | Given a transformation on position trees, apply this transformation to
every position tree in the variable map.

@since wip
-}
vmMap :: (PosTree -> PosTree) -> VarMap -> VarMap
vmMap f (VarMap m) = VarMap . fmap f $ m

{- | Given some way to combine position trees associated with identical
identifiers, merge two 'VarMap's. The \'combining function\' will be used if
an identifier exists in both maps; otherwise, the pair of identifier-tree
will be added as-is.

@since wip
-}
vmMerge :: (PosTree -> PosTree -> PosTree) -> VarMap -> VarMap -> VarMap
vmMerge f (VarMap m1) (VarMap m2) = VarMap . Map.unionWith f m1 $ m2

{- | Given a pair of identifier and position tree, add it to an existing
variable map. If this identifier is already associated to a position tree,
'vmExtend' will overwrite it.

@since wip
-}
vmExtend :: Word64 -> PosTree -> VarMap -> VarMap
vmExtend k v (VarMap m) = VarMap . Map.insert k v $ m

{- | As 'vmMerge', but effectful. This uses a strategy from the merge API of
@Data.Map@ to resolve cases where the same identifier exists in both
'VarMap's.

@since wip
-}
vmMergeM ::
  forall (f :: Type -> Type).
  Applicative f =>
  WhenMatched f Word64 PosTree PosTree PosTree -> VarMap -> VarMap -> f VarMap
vmMergeM f (VarMap m1) (VarMap m2) =
  VarMap <$> mergeA preserveMissing preserveMissing f m1 m2

{- | A fold-with-key over a variable map.

@since wip
-}
vmFold ::
  forall (a :: Type).
  (a -> Word64 -> PosTree -> a) ->
  a ->
  VarMap ->
  a
vmFold f x (VarMap m) = Map.foldlWithKey' f x m
