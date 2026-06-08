{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Plutarch.Test.Suite.Plutarch.POrd (tests) where

import Data.Kind (Type)
import Plutarch.LedgerApi.V1 (
  PCredential (PPubKeyCredential, PScriptCredential),
  PMaybeData,
 )
import Plutarch.Prelude
import Plutarch.Test.Golden (GoldenTestTree, goldenEval, goldenGroup, plutarchGolden)
import Plutarch.Test.Laws (checkHaskellOrdEquivalent, checkPOrdLaws)
import PlutusLedgerApi.QuickCheck.Utils ()
import PlutusLedgerApi.V1 (Credential (PubKeyCredential, ScriptCredential))
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "POrd"
    [ testGroup
        "Goldens"
        [ plutarchGolden
            "lt"
            "pisdata.lt"
            [ goldenGroup
                "PCredential"
                [ goldenGroup "pmatch" (ltWith ltCred c1 c2)
                ]
            ]
        , plutarchGolden
            "lte"
            "pisdata.lte"
            [ goldenGroup
                "PCredential"
                [ goldenGroup "pmatch" (lteWith lteCred c1 c2)
                ]
            ]
        ]
    , testGroup
        "Haskell Equivalence"
        [ checkHaskellOrdEquivalent @PBool
        , checkHaskellOrdEquivalent @(PMaybeData PInteger)
        ]
    , testGroup
        "Laws"
        [ testGroup "PRational" (checkPOrdLaws @PRational)
        ]
    ]

-- Credential utils

c1 :: Credential
c1 = PubKeyCredential ""

c2 :: Credential
c2 = ScriptCredential "41"

ltCred :: Term s PCredential -> Term s PCredential -> Term s PBool
ltCred = pmatchHelperCred (#<)

lteCred :: Term s PCredential -> Term s PCredential -> Term s PBool
lteCred = pmatchHelperCred (#<=)

-- manual 'pmatch' + manual field extraction impl.
pmatchHelperCred ::
  (Term s PByteString -> Term s PByteString -> Term s PBool) ->
  Term s PCredential ->
  Term s PCredential ->
  Term s PBool
pmatchHelperCred f cred1 cred2 = unTermCont $ do
  x <- tcont $ pmatch cred1
  y <- tcont $ pmatch cred2
  pure $ case (x, y) of
    (PPubKeyCredential (pto . pfromData -> a), PPubKeyCredential (pto . pfromData -> b)) -> f a b
    (PPubKeyCredential _, PScriptCredential _) -> pconstant True
    (PScriptCredential _, PPubKeyCredential _) -> pconstant False
    (PScriptCredential (pto . pfromData -> a), PScriptCredential (pto . pfromData -> b)) -> f a b

-- Ord utils

ltWith ::
  forall (p :: S -> Type).
  PLiftable p =>
  (forall (s :: S). Term s p -> Term s p -> Term s PBool) ->
  AsHaskell p ->
  AsHaskell p ->
  [GoldenTestTree]
ltWith f x y =
  [ goldenEval "true" (pconstant @p x `f` pconstant @p y)
  , goldenEval "false" (pconstant @p y `f` pconstant @p x)
  ]

lteWith ::
  forall (p :: S -> Type).
  PLiftable p =>
  (forall (s :: S). Term s p -> Term s p -> Term s PBool) ->
  AsHaskell p ->
  AsHaskell p ->
  [GoldenTestTree]
lteWith f x y =
  [ goldenGroup
      "true"
      [ goldenEval "eq" (pconstant @p x `f` pconstant @p x)
      , goldenEval "less" (pconstant @p x `f` pconstant @p y)
      ]
  , goldenEval "false" (pconstant @p y `f` pconstant @p x)
  ]
