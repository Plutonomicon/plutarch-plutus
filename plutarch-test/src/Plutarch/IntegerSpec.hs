module Plutarch.IntegerSpec (spec) where

import Test.Syd

import Plutarch
import Plutarch.Prelude
import Plutarch.Test

spec :: Spec
spec = do
  describe "int" $ do
    describe "examples" $ do
      goldens
        [ ("add1", popaque add1)
        , ("add1Hoisted", popaque add1Hoisted)
        , ("example1", popaque example1)
        , ("example2", popaque example2)
        , ("fib", popaque fib)
        , ("fib.app.9", popaque $ fib # 9)
        , ("uglyDouble", popaque uglyDouble)
        ]

add1 :: Term s (PInteger :--> PInteger :--> PInteger)
add1 = plam $ \x y -> x + y + 1

add1Hoisted :: Term s (PInteger :--> PInteger :--> PInteger)
add1Hoisted = phoistAcyclic $ plam $ \x y -> x + y + 1

example1 :: Term s PInteger
example1 = add1Hoisted # 12 # 32 + add1Hoisted # 5 # 4

example2 :: Term s (PEither PInteger PInteger :--> PInteger)
example2 = plam $ \x -> pmatch x $ \case
  PLeft n -> n + 1
  PRight n -> n - 1

fib :: Term s (PInteger :--> PInteger)
fib = phoistAcyclic $
  pfix #$ plam $ \self n ->
    pif
      (n #== 0)
      0
      $ pif
        (n #== 1)
        1
        $ self # (n - 1) + self # (n - 2)

uglyDouble :: Term s (PInteger :--> PInteger)
uglyDouble = plam $ \n -> plet n $ \n1 -> plet n1 $ \n2 -> n2 + n2
