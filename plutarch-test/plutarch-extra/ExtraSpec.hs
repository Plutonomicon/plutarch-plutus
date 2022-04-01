module ExtraSpec (spec) where

import qualified Plutarch.ListUtilsSpec as ListUtilsSpec
import Plutarch.Test (TrailSpec)

spec :: TrailSpec
spec = do
  ListUtilsSpec.spec
