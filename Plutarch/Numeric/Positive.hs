module Plutarch.Numeric.Positive (
  Positive (..),
) where

import Data.Coerce (coerce)
import Plutarch.Primitive.Liftable (
  PlutusRepresentable (AsPlutus, haskToRepr, reprToHask),
  ReprError (UnexpectedNegative, UnexpectedZero),
 )

-- | @since wip
newtype Positive = MkUnsafePositive Integer
  deriving
    ( -- | @since wip
      Eq
    , -- | @since wip
      Ord
    )
    via Integer
  deriving stock
    ( -- | @since wip
      Show
    )

-- | @since wip
instance PlutusRepresentable Positive where
  type AsPlutus Positive = Integer
  haskToRepr = coerce
  reprToHask i = case signum i of
    0 -> Left UnexpectedZero
    (-1) -> Left . UnexpectedNegative $ i
    _ -> Right . MkUnsafePositive $ i
