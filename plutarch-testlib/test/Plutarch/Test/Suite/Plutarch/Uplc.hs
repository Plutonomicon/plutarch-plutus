module Plutarch.Test.Suite.Plutarch.Uplc (tests) where

import Plutarch.Internal (punsafeConstantInternal)
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEval, goldenEvalFail, goldenGroup, plutarchGolden)
import Plutarch.Unsafe (punsafeBuiltin)
import PlutusCore qualified as PLC
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "UPLC"
    [ plutarchGolden
        "uplc-behaviour"
        "uplc"
        [ goldenEval
            "2:[1]"
            ( let l :: Term _ (PBuiltinList PInteger) =
                    punsafeConstantInternal . PLC.Some $
                      PLC.ValueOf (PLC.DefaultUniApply PLC.DefaultUniProtoList PLC.DefaultUniInteger) [1]
               in pforce (punsafeBuiltin PLC.MkCons) # (2 :: Term _ PInteger) # l
            )
        , goldenEvalFail
            "fails:True:[1]"
            ( let l :: Term _ (PBuiltinList POpaque) =
                    punsafeConstantInternal . PLC.Some $
                      PLC.ValueOf (PLC.DefaultUniApply PLC.DefaultUniProtoList PLC.DefaultUniInteger) [1]
               in pforce (punsafeBuiltin PLC.MkCons) # pcon PTrue # l
            )
        , goldenEval
            "(2,1)"
            ( punsafeConstantInternal
                . PLC.Some
                $ PLC.ValueOf
                  ( PLC.DefaultUniApply
                      (PLC.DefaultUniApply PLC.DefaultUniProtoPair PLC.DefaultUniInteger)
                      PLC.DefaultUniInteger
                  )
                  (1, 2)
            )
        , goldenEvalFail
            "fails:MkPair-1-2"
            (punsafeBuiltin PLC.MkPairData # (1 :: Term _ PInteger) # (2 :: Term _ PInteger))
        ]
    , plutarchGolden
        "uplc-misc"
        "misc"
        [ goldenEvalFail "perror" perror
        , goldenGroup
            "laziness"
            [ goldenEval "f.d" (pforce . pdelay $ (0 :: Term s PInteger))
            , goldenEval "d.f.d" (pdelay . pforce . pdelay $ (0 :: Term s PInteger))
            ]
        , goldenGroup
            "hoist"
            [ goldenEval "id.0" (phoistAcyclic $ plam $ \x -> x # (0 :: Term s PInteger))
            , goldenEval "fstPair" (phoistAcyclic (punsafeBuiltin PLC.FstPair))
            ]
        ]
    ]
