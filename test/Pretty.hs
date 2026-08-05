module Pretty (goldens) where

import Data.Text qualified as Text
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  plam',
  punsafeConstant,
 )
import Plutarch.Primitive.Apply ((#))
import Plutarch.Primitive.Bool (PBool)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Liftable (pconstant)
import Plutarch.Primitive.List (PBList)
import Plutarch.Primitive.Numeric (PInteger)
import Plutarch.Primitive.Pair (PBPair)
import Plutarch.Primitive.String (PString)
import Plutarch.Test.Golden (plutarchGolden, plutarchGoldenEval)
import PlutusCore qualified as PLC
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutSmart)
import Prettyprinter.Render.String (renderString)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Pretty"
    [ plutarchGolden "[[(2, 3)]]" "Pretty Case 1" case1
    , plutarchGoldenEval "[[(2, 3)]]" "Pretty Case 1" case1
    , plutarchGolden
        "\\f x -> let lol = toPrettyString (f # x) in pconstant (T.pack . show $ lol)"
        "Pretty Case 2"
        case2
    , plutarchGoldenEval
        "\\f x -> let lol = toPrettyString (f # x) in pconstant (T.pack . show $ lol)"
        "Pretty Case 2"
        case2
    ]

-- Cases

-- Case 1: [[2]] (for pretty printing)
case1 :: forall (s :: S). Term s (PBList (PBList (PBPair PInteger PInteger)))
case1 = punsafeConstant $ PLC.someValue @[[(Integer, Integer)]] [[(2, 3)]]

-- Case 2: \f x -> let lol = toPrettyString (f # x) in pconstant (T.pack . show $ lol)
case2 :: forall (s :: S). Term s ((PBool :--> PBool) :--> PBool :--> PString)
case2 = plam' $ \f -> plam' $ \x ->
  let lol = toPrettyString (f # x)
   in pconstant (Text.pack . show $ lol)

toPrettyString :: forall a. Pretty a => a -> String
toPrettyString = renderString . layoutSmart defaultLayoutOptions . pretty
