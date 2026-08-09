module Debug (goldens) where

import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  debugTermEnv,
  releaseTermEnv,
 )
import Plutarch.Primitive.Bool (ptrue)
import Plutarch.Primitive.Debug (pshow)
import Plutarch.Primitive.String (PString)
import Plutarch.Test.Golden (
  plutarchGoldenEvalWith,
  plutarchGoldenWith,
 )
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Debug"
    [ plutarchGoldenWith debugTermEnv "pshow Bool" "Debug Case 1" case1
    , plutarchGoldenEvalWith debugTermEnv "pshow Bool" "Debug Case 1" case1
    , plutarchGoldenWith releaseTermEnv "pshow Bool" "Debug Case 1 tracing off" case1
    , plutarchGoldenEvalWith releaseTermEnv "pshow Bool" "Debug Case 1 tracing off" case1
    ]

-- Cases

case1 :: forall (s :: S). Term s PString
case1 = pshow ptrue
