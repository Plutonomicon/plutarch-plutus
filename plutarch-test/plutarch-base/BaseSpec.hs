module BaseSpec (spec) where

import qualified Plutarch.ApiSpec as ApiSpec
import qualified Plutarch.BoolSpec as BoolSpec
import qualified Plutarch.ByteStringSpec as ByteStringSpec
import qualified Plutarch.EitherSpec as EitherSpec
import qualified Plutarch.IntegerSpec as IntegerSpec
import qualified Plutarch.LiftSpec as LiftSpec
import qualified Plutarch.ListSpec as ListSpec
import qualified Plutarch.MaybeSpec as MaybeSpec
import qualified Plutarch.PIsDataSpec as PIsDataSpec
import qualified Plutarch.PLamSpec as PLamSpec
import qualified Plutarch.POrdSpec as POrdSpec
import qualified Plutarch.PairSpec as PairSpec
import qualified Plutarch.PlutusTypeSpec as PlutusTypeSpec
import qualified Plutarch.RationalSpec as RationalSpec
import qualified Plutarch.RecSpec as RecSpec
import qualified Plutarch.RecursionSpec as RecursionSpec
import qualified Plutarch.ScriptsSpec as ScriptsSpec
import qualified Plutarch.ShowSpec as ShowSpec
import qualified Plutarch.StringSpec as StringSpec
import Plutarch.Test (TrailSpec)
import qualified Plutarch.TraceSpec as TraceSpec
import qualified Plutarch.UPLCSpec as UPLCSpec
import qualified Plutarch.UnitSpec as UnitSpec

spec :: TrailSpec
spec = do
  ApiSpec.spec
  BoolSpec.spec
  ByteStringSpec.spec
  EitherSpec.spec
  IntegerSpec.spec
  LiftSpec.spec
  ListSpec.spec
  MaybeSpec.spec
  PairSpec.spec
  PIsDataSpec.spec
  PLamSpec.spec
  PlutusTypeSpec.spec
  POrdSpec.spec
  RationalSpec.spec
  RecursionSpec.spec
  ScriptsSpec.spec
  ShowSpec.spec
  RecSpec.spec
  StringSpec.spec
  TraceSpec.spec
  UnitSpec.spec
  UPLCSpec.spec
