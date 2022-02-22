{-# LANGUAGE QualifiedDo #-}

module Main (main) where

import Plutarch (ClosedTerm)
import Plutarch.Api.V1
import Plutarch.Benchmark (NamedBenchmark, bench, benchGroup, benchMain)
import Plutarch.Bool
import Plutarch.Builtin
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import Plutus.V1.Ledger.Address (Address (Address))
import Plutus.V1.Ledger.Api (DCert (DCertGenesis), toData)
import Plutus.V1.Ledger.Contexts (ScriptPurpose (Certifying, Minting, Rewarding, Spending), TxOutRef (TxOutRef))
import Plutus.V1.Ledger.Credential (
  Credential (PubKeyCredential, ScriptCredential),
  StakingCredential (StakingPtr),
 )

main :: IO ()
main = do
  benchMain benchmarks

benchmarks :: [NamedBenchmark]
benchmarks =
  benchGroup
    "types"
    [ benchGroup "data" dataBench
    ]

dataBench :: [[NamedBenchmark]]
dataBench =
  [ benchGroup
      "pmatch-pfield"
      -- These two should ideally have the exact same efficiency.
      [ benchGroup
          "pmatch"
          [ bench "newtype" $ P.do
              let addr = pconstant $ Address (PubKeyCredential "ab") Nothing
              PAddress addrFields <- pmatch addr
              y <- pletFields @'["credential", "stakingCredential"] addrFields
              ppairDataBuiltin # hrecField @"credential" y # hrecField @"stakingCredential" y
          ]
      , benchGroup
          "pfield"
          [ bench "newtype" $ P.do
              let addr = pconstant $ Address (PubKeyCredential "ab") Nothing
              y <- pletFields @'["credential", "stakingCredential"] addr
              ppairDataBuiltin # hrecField @"credential" y # hrecField @"stakingCredential" y
          ]
      ]
  , benchGroup
      "pfield-pletFields"
      -- These two should ideally have the exact same efficiency.
      [ benchGroup
          "pfield"
          [ bench "single" $ P.do
              let addr = pconstant $ Address (PubKeyCredential "ab") Nothing
              pfromData $ pfield @"credential" # addr
          ]
      , benchGroup
          "pletFields"
          [ bench "single" $ P.do
              let addr = pconstant $ Address (PubKeyCredential "ab") Nothing
              y <- pletFields @'["credential"] addr
              pfromData $ hrecField @"credential" y
          ]
      ]
  ]
