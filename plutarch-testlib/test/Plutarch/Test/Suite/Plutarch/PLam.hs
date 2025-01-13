module Plutarch.Test.Suite.Plutarch.PLam (tests) where

import Data.ByteString (ByteString)
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEval, goldenGroup, plutarchGolden)
import Plutarch.Unsafe (punsafeBuiltin)
import PlutusCore qualified as PLC
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "PLam"
    [ plutarchGolden
        "Goldens"
        "plam"
        [ goldenEval "id" (plam id)
        , goldenEval "flip.const" (plam (\_ y -> y))
        , goldenEval "plet" (plam (\x _ -> plet x $ const perror))
        , goldenGroup
            "primitives"
            [ goldenGroup
                "bool"
                [ goldenEval "true" (plam $ \_ -> pconstant @PBool True)
                ]
            , goldenGroup
                "int"
                [ goldenEval "0" (plam $ const (0 :: Term _ PInteger))
                , goldenEval "1" (plam $ const (1 :: Term _ PInteger))
                , goldenEval "512" (plam $ const (512 :: Term _ PInteger))
                , goldenEval "1048576" (plam $ const (1048576 :: Term _ PInteger))
                ]
            , goldenGroup
                "bytestring"
                [ goldenEval "1" (plam $ \_ -> pconstant @PByteString ("1" :: ByteString))
                , goldenEval "1111111" (plam $ \_ -> pconstant @PByteString ("1111111" :: ByteString))
                ]
            , goldenGroup
                "unit"
                [ goldenEval "list" (plam $ \_ -> pconstant @(PBuiltinList PUnit) ([()] :: [()]))
                , goldenEval "()" (plam $ \_ -> pconstant @PUnit ())
                ]
            , goldenEval "id" (plam id)
            , goldenGroup
                "fun"
                [ goldenEval "lam+" (plam $ const (plam (+) :: Term _ (PInteger :--> PInteger :--> PInteger)))
                , goldenEval "+" (plam (+) :: Term _ (PInteger :--> PInteger :--> PInteger))
                ]
            ]
        , goldenGroup
            "η-reduction-optimisations"
            [ goldenEval "λx y. addInteger x y => addInteger" (plam $ \x y -> (x :: Term _ PInteger) + y)
            , goldenEval "λx y. hoist (force mkCons) x y => force mkCons" (plam $ \x y -> pforce (punsafeBuiltin PLC.MkCons) # x # y)
            , goldenEval "λx y. hoist mkCons x y => mkCons x y" (plam $ \x y -> punsafeBuiltin PLC.MkCons # x # y)
            , goldenEval "λx y. hoist (λx y. x + y - y - x) x y => λx y. x + y - y - x" (plam $ \x y -> phoistAcyclic (plam $ \(x :: Term _ PInteger) y -> x + y - y - x) # x # y)
            , goldenEval "λx y. x + x" (plam $ \(x :: Term _ PInteger) (_ :: Term _ PInteger) -> x + x)
            , goldenEval "let x = addInteger in x 1 1" (plet (punsafeBuiltin PLC.AddInteger) $ \x -> x # (1 :: Term _ PInteger) # (1 :: Term _ PInteger))
            , goldenEval "let x = 0 in x => 0" (plet 0 $ \(x :: Term _ PInteger) -> x)
            , goldenEval "let x = hoist (λx. x + x) in 0 => 0" (plet (phoistAcyclic $ plam $ \(x :: Term _ PInteger) -> x + x) $ const (0 :: Term _ PInteger))
            , goldenEval "let x = hoist (λx. x + x) in x" (plet (phoistAcyclic $ plam $ \(x :: Term _ PInteger) -> x + x) id)
            , goldenEval "λx y. sha2_256 x y =>!" (plam (\x y -> punsafeBuiltin PLC.Sha2_256 # x # y))
            , goldenEval "let f = hoist (λx. x) in λx y. f x y => λx y. x y" (plam (\x y -> phoistAcyclic (plam id) # x # y))
            , goldenEval "let f = hoist (λx. x True) in λx y. f x y => λx y. (λz. z True) x y" (plam (\x y -> phoistAcyclic (plam $ \x -> x # pcon PTrue) # x # y))
            , goldenEval "λy. (λx. x + x) y" (plam $ \y -> plam (\(x :: Term _ PInteger) -> x + x) # y)
            ]
        ]
    ]
