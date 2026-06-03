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

data PosTree
  = PHere
  | POne PosTree
  | PTwo (These PosTree PosTree)
  | PMany (Vector (Maybe PosTree))
  | PApplyCase (Maybe PosTree) (NonEmptyVector (Maybe PosTree))
  deriving stock (Show, Eq)

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
