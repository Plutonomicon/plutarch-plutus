module Plutarch.Backend.VarMap (
  VarMap,
  vmEmpty,
  vmSingleton,
  vmDelete,
  vmMap,
  vmMerge,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Plutarch.Backend.PosTree (PosTree)

newtype VarMap = VarMap (Map Word64 PosTree)
  deriving (Eq) via (Map Word64 PosTree)
  deriving stock (Show)

vmEmpty :: VarMap
vmEmpty = VarMap Map.empty

vmSingleton :: Word64 -> PosTree -> VarMap
vmSingleton k = VarMap . Map.singleton k

vmDelete :: Word64 -> VarMap -> (Maybe PosTree, VarMap)
vmDelete k (VarMap m) = (Map.lookup k m, VarMap . Map.delete k $ m)

vmMap :: (PosTree -> PosTree) -> VarMap -> VarMap
vmMap f (VarMap m) = VarMap . fmap f $ m

vmMerge :: (PosTree -> PosTree -> PosTree) -> VarMap -> VarMap -> VarMap
vmMerge f (VarMap m1) (VarMap m2) = VarMap . Map.unionWith f m1 $ m2
