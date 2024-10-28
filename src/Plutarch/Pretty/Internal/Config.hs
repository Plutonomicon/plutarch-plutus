module Plutarch.Pretty.Internal.Config (keywords, indentWidth, forcedPrefix) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)

import Prettyprinter qualified as PP

import PlutusCore qualified as PLC

keywords :: Set Text
keywords =
  Set.fromList $
    ["let", "in"]
      <> fmap (fromString . show . PP.pretty) [(minBound @PLC.DefaultFun) .. maxBound]

indentWidth :: Int
indentWidth = 2

-- | Prefix to use for naming forced builtin functions.
forcedPrefix :: Text
forcedPrefix = "fr"
