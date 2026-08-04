{-# LANGUAGE OverloadedLists #-}

module Error (goldens) where

import Data.Kind (Type)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  perror,
  plam',
  punsafeCase,
  punsafeConstr,
  toSomeTerm,
 )
import Plutarch.Primitive.Apply ((#), (#$))
import Plutarch.Primitive.BuiltinFun (paddInteger)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Numeric (PInteger)
import Plutarch.Test.Golden (plutarchGolden, plutarchGoldenEval)
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Error"
    [ plutarchGolden "\\x y -> addInteger x error (addInteger x error)" "Error Case 1" case1
    , plutarchGoldenEval "\\x y -> addInteger x error (addInteger x error)" "Error Case 1" case1
    , plutarchGolden "\\x -> constr 0 [x, error]" "Error Case 2" case2
    , plutarchGoldenEval "\\x -> constr 0 [x, error]" "Error Case 2" case2
    , plutarchGolden "\\x -> case error of [x]" "Error Case 3" case3
    , plutarchGoldenEval "\\x -> case error of [x]" "Error Case 3" case3
    ]

-- Cases

-- Case 1: \x -> addInteger error (addInteger x error)
case1 :: forall (s :: S). Term s (PInteger :--> PInteger)
case1 = plam' $ \x -> paddInteger # perror #$ paddInteger # x # perror

-- Case 2: \x -> constr 0 [x, error]
case2 :: forall (a :: S -> Type) (b :: S -> Type) (s :: S). Term s (a :--> b)
case2 = plam' $ \x -> punsafeConstr 0 [toSomeTerm x, toSomeTerm perror]

-- Case 3: \x -> case error of [x]
case3 :: forall (a :: S -> Type) (s :: S). Term s (a :--> a)
case3 = plam' $ \x -> punsafeCase perror . NEVector.singleton . toSomeTerm $ x
