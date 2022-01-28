{-# LANGUAGE QualifiedDo #-}

module Main (main) where

import Data.ByteString (ByteString)
import Plutarch.Api.V1
import Plutarch.Benchmark (NamedBenchmark, bench, bench', benchGroup, benchMain)
import Plutarch.Bool
import Plutarch.Builtin
import qualified Plutarch.List as List
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import Plutus.V1.Ledger.Address (Address (Address))
import Plutus.V1.Ledger.Api (toData)
import Plutus.V1.Ledger.Contexts (ScriptPurpose (Minting, Spending), TxOutRef (TxOutRef))
import Plutus.V1.Ledger.Credential (Credential (PubKeyCredential, ScriptCredential))

main :: IO ()
main = do
  benchMain benchmarks

benchmarks :: [NamedBenchmark]
benchmarks =
  benchGroup
    "types"
    [ benchGroup "int" integerBench
    , benchGroup "bool" boolBench
    , benchGroup "builtin:intlist" intListBench
    , benchGroup "data" dataBench
    ]

integerBench :: [[NamedBenchmark]]
integerBench =
  [ -- Calling add twice
    benchGroup
      "add(2)"
      $ let addInlined :: Term s PInteger -> Term s PInteger -> Term s PInteger
            addInlined x y = x + y + 1
            addUnhoisted :: Term s (PInteger :--> PInteger :--> PInteger)
            addUnhoisted = plam $ \x y -> x + y + 1
            addHoisted :: Term s (PInteger :--> PInteger :--> PInteger)
            addHoisted = phoistAcyclic $ plam $ \x y -> x + y + 1
         in [ bench "inlined" $ addInlined 12 32 + addInlined 5 4
            , bench "unhoist" $ addUnhoisted # 12 # 32 + addUnhoisted # 5 # 4
            , bench "hoisted" $ addHoisted # 12 # 32 + addHoisted # 5 # 4
            ]
  ]

boolBench :: [[NamedBenchmark]]
boolBench =
  let true = pconstant @PBool True
      false = pconstant @PBool False
      pandNoHoist = phoistAcyclic $ plam $ \x y -> pif' # x # y # (pdelay $ pcon PFalse)
   in [ benchGroup
          "and"
          [ bench "strict" $ pand' # true # false
          , bench "lazy" $ (#&&) true false
          , -- Calling `pand` twice.
            bench "pand(2)" $
              let x = pand # true # pdelay false
               in pand # true # x
          , bench "pand(2):unhoisted" $
              let x = pandNoHoist # true # pdelay false
               in pandNoHoist # true # x
          ]
      ]

intListBench :: [[NamedBenchmark]]
intListBench =
  let numList = pconstant @(PBuiltinList PInteger) [1 .. 5]
   in [ bench "phead" $ List.phead # numList
      , bench "ptail" $ List.ptail # numList
      , -- Accessing the first two elements, and adds them.
        benchGroup
          "x1+x2"
          [ -- Via HeadList and TailList only
            bench "builtin" $
              (List.phead #$ List.ptail # numList) + (List.phead # numList)
          , -- Via ChooseList (twice invoked)
            bench "pmatch" $
              pmatch numList $ \case
                PNil -> perror
                PCons x xs ->
                  pmatch xs $ \case
                    PNil -> perror
                    PCons y _ ->
                      x + y
          ]
      , -- Various ways to uncons a list
        benchGroup
          "uncons"
          [ -- ChooseList builtin, like uncons but fails on null lists
            bench "ChooseList" $
              pmatch numList $ \case
                PNil -> perror
                PCons _x xs ->
                  xs
          , -- Retrieving head and tail of a list
            bench "head-and-tail" $
              plet (List.phead # numList) $ \_x ->
                List.ptail # numList
          , -- Retrieve head and tail using builtins, but fail on null lists.
            bench "head-and-tail-and-null" $
              plet (List.pnull # numList) $ \isEmpty ->
                pmatch isEmpty $ \case
                  PTrue -> perror
                  PFalse -> plet (List.phead # numList) $ \_x ->
                    List.ptail # numList
          ]
      , bench
          "plength"
          $ List.plength # pconstant @(PBuiltinList PInteger) [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]
      , bench
          "pelem"
          $ List.pelem # 1 # pconstant @(PBuiltinList PInteger) [5, 2, 3, 4, 7, 5, 1, 6, 2]
      , bench
          "pall"
          $ List.pall @PBuiltinList @PInteger # plam (const $ pconstant @PBool False) # pconstant [1, 2, 3, 4, 5, 6]
      , benchGroup
          "plistEquals"
          [ bench "==(n=3)" $ List.plistEquals @PBuiltinList @PInteger # pconstant [1, 2, 3] # pconstant [1, 2, 3]
          , bench "/=(n=4)" $ List.plistEquals @PBuiltinList @PInteger # pconstant [1, 2, 3, 4] # pconstant [1, 2, 3]
          , bench "/=(empty;n=3)" $ List.plistEquals @PBuiltinList @PInteger # pconstant [] # pconstant [1, 2, 3]
          ]
      , benchGroup
          "primitives"
          [ bench' $ plam $ \_ -> pconstant True
          , bench' $ plam $ \_ -> (0 :: Term _ PInteger)
          , bench' $ plam $ \_ -> (1 :: Term _ PInteger)
          , bench' $ plam $ \_ -> (512 :: Term _ PInteger)
          , bench' $ plam $ \_ -> (1048576 :: Term _ PInteger)
          , bench' $ plam $ \_ -> pconstant ("1" :: ByteString)
          , bench' $ plam $ \_ -> pconstant ("1111111" :: ByteString)
          , bench' $ plam $ \_ -> pconstant ([()] :: [()])
          , bench' $ plam $ \_ -> pconstant ()
          , bench' $ pconstant ()
          , bench' $ plam $ \x -> x
          , bench' $ plam $ \_ -> (plam (+) :: Term _ (PInteger :--> PInteger :--> PInteger))
          , bench' $ (plam (+) :: Term _ (PInteger :--> PInteger :--> PInteger))
          ]
      ]

dataBench :: [[NamedBenchmark]]
dataBench =
  [ benchGroup "deconstruction" deconstrBench
  , benchGroup
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

{- | For comparing typed and untyped data deconstruction approaches.

We ideally want the typed and raw versions to have as little deviation as possible.
-}
deconstrBench :: [[NamedBenchmark]]
deconstrBench =
  [ benchGroup
      "matching"
      $ let addr = Address (PubKeyCredential "ab") Nothing
            minting = Minting ""
            spending = Spending (TxOutRef "ab" 0)
         in [ benchGroup
                "typed"
                [ bench "newtype" $
                    plam
                      ( \x -> P.do
                          PAddress addrFields <- pmatch x
                          addrFields
                      )
                      # pconstant addr
                , bench "sumtype(ignore-fields)" $
                    plam
                      ( \x -> P.do
                          PMinting _ <- pmatch x
                          pconstant ()
                      )
                      # pconstant minting
                , bench "sumtype(partial-match)" $
                    plam
                      ( \x -> P.do
                          PMinting hs <- pmatch x
                          hs
                      )
                      # pconstant minting
                , bench "sumtype(exhaustive)" $
                    plam
                      ( \x -> P.do
                          purp <- pmatch x
                          case purp of
                            PMinting f -> plet f $ const $ phexByteStr "01"
                            PSpending f -> plet f $ const $ phexByteStr "02"
                            PRewarding f -> plet f $ const $ phexByteStr "03"
                            PCertifying f -> plet f $ const $ phexByteStr "04"
                      )
                      # pconstant spending
                , bench "sumtype(exhaustive)(ignore-fields)" $
                    plam
                      ( \x -> P.do
                          purp <- pmatch x
                          case purp of
                            PMinting _ -> phexByteStr "01"
                            PSpending _ -> phexByteStr "02"
                            PRewarding _ -> phexByteStr "03"
                            PCertifying _ -> phexByteStr "04"
                      )
                      # pconstant spending
                ]
            , benchGroup
                "raw"
                [ bench "newtype" $
                    plam
                      ( \x ->
                          psndBuiltin #$ pasConstr # x
                      )
                      #$ pconstant
                      $ toData addr
                , bench "sumtype(ignore-fields)" $
                    plam
                      ( \x ->
                          pif
                            ((pfstBuiltin #$ pasConstr # x) #== 0)
                            (pconstant ())
                            perror
                      )
                      #$ pconstant
                      $ toData minting
                , bench "sumtype(partial-match)" $
                    plam
                      ( \x ->
                          plet (pasConstr # x) $ \d ->
                            pif
                              (pfstBuiltin # d #== 0)
                              (psndBuiltin # d)
                              perror
                      )
                      #$ pconstant
                      $ toData minting
                , bench "sumtype(exhaustive)" $
                    plam
                      ( \x -> P.do
                          d <- plet $ pasConstr # x
                          constr <- plet $ pfstBuiltin # d
                          fields <- plet $ psndBuiltin # d
                          pif
                            (constr #== 0)
                            (plet fields $ const $ phexByteStr "01")
                            $ pif
                              (constr #== 1)
                              (plet fields $ const $ phexByteStr "02")
                              $ pif
                                (constr #== 2)
                                (plet fields $ const $ phexByteStr "03")
                                $ plet fields $ const $ phexByteStr "04"
                      )
                      #$ pconstant
                      $ toData spending
                , bench "sumtype(exhaustive)(ignore-fields)" $
                    plam
                      ( \x -> P.do
                          d <- plet $ pasConstr # x
                          constr <- plet $ pfstBuiltin # d
                          pif
                            (constr #== 0)
                            (phexByteStr "01")
                            $ pif
                              (constr #== 1)
                              (phexByteStr "02")
                              $ pif
                                (constr #== 2)
                                (phexByteStr "03")
                                $ phexByteStr "04"
                      )
                      #$ pconstant
                      $ toData spending
                ]
            ]
  , benchGroup
      "fields"
      $ let addr = Address (ScriptCredential "ab") Nothing
         in [ benchGroup
                "typed"
                [ bench "extract-single" $
                    plam
                      ( \x ->
                          pfield @"credential" # x
                      )
                      # pconstant addr
                ]
            , benchGroup
                "raw"
                [ bench "extract-single" $
                    plam
                      ( \x ->
                          phead #$ psndBuiltin #$ pasConstr # x
                      )
                      #$ pconstant
                      $ toData addr
                ]
            ]
  , benchGroup
      "combined"
      $ let addr = Address (ScriptCredential "ab") Nothing
         in [ benchGroup
                "typed"
                [ bench "toValidatorHash" $
                    plam
                      ( \x -> P.do
                          cred <- pmatch . pfromData $ pfield @"credential" # x
                          case cred of
                            PPubKeyCredential _ -> pcon PNothing
                            PScriptCredential credFields -> pcon . PJust $ pto $ pfromData $ pfield @"_0" # credFields
                      )
                      # pconstant addr
                ]
            , benchGroup
                "raw"
                [ bench "toValidatorHash" $
                    plam
                      ( \x ->
                          P.do
                            let cred = phead #$ psndBuiltin #$ pasConstr # x
                            deconstrCred <- plet $ pasConstr # cred
                            pif
                              (pfstBuiltin # deconstrCred #== 0)
                              (pcon PNothing)
                              $ pcon . PJust $ pasByteStr #$ phead #$ psndBuiltin # deconstrCred
                      )
                      #$ pconstant
                      $ toData addr
                ]
            ]
  ]
