module Debug (goldens) where

import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  debugTermEnv,
  releaseTermEnv,
 )
import Plutarch.Primitive.Bool (ptrue)
import Plutarch.Primitive.ByteString (PByteString)
import Plutarch.Primitive.Debug (pshow)
import Plutarch.Primitive.Liftable (pconstant)
import Plutarch.Primitive.List (PBList)
import Plutarch.Primitive.Numeric (PByte, PInteger)
import Plutarch.Primitive.Pair (PBPair)
import Plutarch.Primitive.String (PString)
import Plutarch.Test.Golden (plutarchGoldenAllWith)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Debug"
    [ plutarchGoldenAllWith debugTermEnv "pshow Bool" "Debug Case 1" case1
    , plutarchGoldenAllWith releaseTermEnv "pshow Bool" "Debug Case 1 tracing off" case1
    , plutarchGoldenAllWith debugTermEnv "pshow Integer" "Debug Case 2" case2
    , plutarchGoldenAllWith debugTermEnv "pshow Byte" "Debug Case 3" case3
    , plutarchGoldenAllWith debugTermEnv "pshow ByteString" "Debug Case 4" case4
    , plutarchGoldenAllWith debugTermEnv "pshow [Integer]" "Debug Case 5" case5
    , plutarchGoldenAllWith debugTermEnv "pshow (Integer, ByteString)" "Debug Case 6" case6
    ]

-- Cases

case1 :: forall (s :: S). Term s PString
case1 = pshow ptrue

case2 :: forall (s :: S). Term s PString
case2 = pshow . pconstant @PInteger $ -12345

case3 :: forall (s :: S). Term s PString
case3 = pshow . pconstant @PByte $ 0x1F

case4 :: forall (s :: S). Term s PString
case4 = pshow . pconstant @PByteString $ "cat goes nya"

case5 :: forall (s :: S). Term s PString
case5 = pshow . pconstant @(PBList PInteger) $ [1, 2, 3, 4]

case6 :: forall (s :: S). Term s PString
case6 = pshow . pconstant @(PBPair PInteger PByteString) $ (10, "nyaaa")
