{-# LANGUAGE QualifiedDo #-}

module Plutarch.Test.Suite.Plutarch.Monadic (tests) where

import Control.Monad.Trans.Cont (cont, runCont)
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V1 (
  PAddress (PAddress),
  PCredential,
  PStakingCredential,
 )
import Plutarch.List (pconvertLists)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEval, plutarchGolden)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Monadic"
    [ testGroup
        "Goldens"
        [ plutarchGolden
            "pmatch-twice"
            "monadic.pmatch-twice"
            [ goldenEval "normal" $ do
                let integerList :: [Integer] -> Term s (PList PInteger)
                    integerList xs = pconvertLists #$ pconstant @(PBuiltinList PInteger) xs
                    xs = integerList [1 .. 10]
                pmatch xs $
                  \case
                    PSCons _x xs' -> do
                      pmatch xs' $ \case
                        PSCons _ xs'' -> xs''
                        PSNil -> perror
                    PSNil -> perror
            , goldenEval "do" $ do
                let integerList :: [Integer] -> Term s (PList PInteger)
                    integerList xs = pconvertLists #$ pconstant @(PBuiltinList PInteger) xs
                    xs = integerList [1 .. 10]
                P.do
                  PSCons _ xs' <- pmatch xs
                  PSCons _ xs'' <- pmatch xs'
                  xs''
            , goldenEval "cont" $ do
                let integerList :: [Integer] -> Term s (PList PInteger)
                    integerList xs = pconvertLists #$ pconstant @(PBuiltinList PInteger) xs
                    xs = integerList [1 .. 10]
                flip runCont id $ do
                  ls <- cont $ pmatch xs
                  case ls of
                    PSCons _ xs' -> do
                      ls' <- cont $ pmatch xs'
                      case ls' of
                        PSCons _ xs'' -> pure xs''
                        PSNil -> pure perror
                    PSNil -> pure perror
            , goldenEval "termcont" $ do
                let integerList :: [Integer] -> Term s (PList PInteger)
                    integerList xs = pconvertLists #$ pconstant @(PBuiltinList PInteger) xs
                    xs = integerList [1 .. 10]
                unTermCont $ do
                  PSCons _ xs' <- TermCont $ pmatch xs
                  PSCons _ xs'' <- TermCont $ pmatch xs'
                  pure xs''
            ]
        ]
    , plutarchGolden
        "api.example.getFields"
        "api.example.getFields"
        [ goldenEval "0" getFields
        ]
    ]

getFields :: Term s (PAddress :--> PDataRecord '["credential" ':= PCredential, "stakingCredential" ':= PMaybeData PStakingCredential])
getFields = phoistAcyclic $
  plam $ \addr -> P.do
    PAddress addrFields <- pmatch addr
    addrFields
