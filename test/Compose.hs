module Compose (goldens) where

import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  pcompose,
  plam',
  punsafeConstant,
 )
import Plutarch.Primitive.Apply ((#))
import Plutarch.Primitive.BuiltinFun (paddInteger, pmultiplyInteger, psubtractInteger)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Numeric (PInteger)
import Plutarch.Test.Codegen (identicalCode)
import Plutarch.Test.Golden (plutarchGolden)
import PlutusCore qualified as PLC
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Compose"
    [ plutarchGolden "\\x -> (compose [\\y -> y + 2, \\z -> x * z, \\z1 -> z1 - 5]) x, left assoc" "Compose Case 1" case1
    , plutarchGolden "\\x -> (compose [\\y -> y + 2, \\z -> x * z, \\z1 -> z1 - 5]) x, right assoc" "Compose Case 2" case2
    , plutarchGolden "\\x -> (compose [\\y -> y + 2, \\z -> z + 2, \\z1 -> z1 + 2]) x" "Compose Case 3" case3
    , plutarchGolden "\\x -> (compose [\\y -> y * y, \\z -> z + 2]) x" "Compose Case 4" case4
    , identicalCode "Case 1 and Case 2" case1 case2
    ]

-- Cases

-- Case 1: \x -> (compose [\y -> y + 2, \z -> x * z, \z1 -> z1 - 5]) x
--
-- Constructed left associatively
case1 :: forall (s :: S). Term s (PInteger :--> PInteger)
case1 =
  let fun1 = plam' $ \y -> paddInteger # y # punsafeConstant (PLC.someValue @Integer 2)
      fun3 = plam' $ \z1 -> psubtractInteger # z1 # punsafeConstant (PLC.someValue @Integer 5)
   in plam' $ \x ->
        let fun2 = plam' $ \z -> pmultiplyInteger # x # z
         in pcompose (pcompose fun1 fun2) fun3 # x

-- Case 2: \x -> (compose [\y -> y + 2, \z -> x * z, \z1 -> z1 - 5]) x
--
-- Constructed right associatively
case2 :: forall (s :: S). Term s (PInteger :--> PInteger)
case2 =
  let fun1 = plam' $ \y -> paddInteger # y # punsafeConstant (PLC.someValue @Integer 2)
      fun3 = plam' $ \z1 -> psubtractInteger # z1 # punsafeConstant (PLC.someValue @Integer 5)
   in plam' $ \x ->
        let fun2 = plam' $ \z -> pmultiplyInteger # x # z
         in (pcompose fun1 . pcompose fun2 $ fun3) # x

-- Case 3: \x -> (compose [\y -> y + 2, \z -> z + 2, \z1 -> z1 + 2]) x
case3 :: forall (s :: S). Term s (PInteger :--> PInteger)
case3 =
  let fun = plam' $ \y -> paddInteger # y # punsafeConstant (PLC.someValue @Integer 2)
   in plam' $ \x -> (pcompose fun . pcompose fun $ fun) # x

-- Case 4: \x -> (compose [\y -> y * y, \z -> z + 2]) x
case4 :: forall (s :: S). Term s (PInteger :--> PInteger)
case4 =
  let f = plam' $ \y -> pmultiplyInteger # y # y
      g = plam' $ \z -> paddInteger # z # punsafeConstant (PLC.someValue @Integer 2)
   in plam' $ \x -> pcompose f g # x
