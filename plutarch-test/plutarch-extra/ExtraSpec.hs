module ExtraSpec (spec) where

import qualified Plutarch.ListUtilsSpec as ListUtilsSpec
import Plutarch.Test

spec :: Spec
spec = do
  ListUtilsSpec.spec
