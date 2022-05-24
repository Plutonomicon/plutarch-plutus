module Plutarch.Pretty.Internal.Config (keywords, indentWidth, forcedPrefix) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)

import qualified Prettyprinter as PP

import qualified PlutusCore as PLC

keywords :: Set Text
keywords =
  Set.fromList $
    ["let", "in"]
      <> map (fromString . show . PP.pretty) [(minBound @PLC.DefaultFun) .. maxBound]

indentWidth :: Int
indentWidth = 2

-- | Prefix to use for naming forced builtin functions.
forcedPrefix :: Text
forcedPrefix = "fr"
