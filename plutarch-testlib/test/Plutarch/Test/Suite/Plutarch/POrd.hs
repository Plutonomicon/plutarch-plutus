{-# LANGUAGE AllowAmbiguousTypes #-}

module Plutarch.Test.Suite.Plutarch.POrd (tests) where

import Data.Kind (Type)
import Plutarch.LedgerApi.V1 (
  PCredential (PPubKeyCredential, PScriptCredential),
  PMaybeData,
 )
import Plutarch.Prelude
import Plutarch.Test.Golden (GoldenTestTree, goldenEval, goldenGroup, plutarchGolden)
import Plutarch.Test.Laws (checkHaskellOrdEquivalent, checkPOrdLaws)
import Plutarch.Test.SpecTypes (PTriplet (PTriplet), Triplet (Triplet))
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
            , goldenGroup
                "PTriplet"
                [ goldenGroup "pmatch" (ltWith ltTrip t1 t2)
                ]
            ]
        , plutarchGolden
            "lte"
            "pisdata.lte"
            [ goldenGroup
                "PCredential"
                [ goldenGroup "pmatch" (lteWith lteCred c1 c2)
                ]
            , goldenGroup
                "PTriplet"
                [ goldenGroup "pmatch" (lteWith lteTrip t1 t2)
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

-- NOTE(Seungheon, Jan 25): We no longer provide default Ord for PDataStruct/PDataRec
-- ltCred' :: Term s PCredential -> Term s PCredential -> Term s PBool
-- ltCred' = pmatchDataRecHelperCred (#<)

-- lteCred' :: Term s PCredential -> Term s PCredential -> Term s PBool
-- lteCred' = pmatchDataRecHelperCred (#<=)

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

-- manual 'pmatch' + 'PDataRecord' Ord impl.
-- NOTE(Seungheon, Jan 25): We no longer provide default Ord for PDataStruct/PDataRec
-- pmatchDataRecHelperCred ::
--   (forall l. POrd (PDataRecord l) => Term s (PDataRecord l) -> Term s (PDataRecord l) -> Term s PBool) ->
--   Term s PCredential ->
--   Term s PCredential ->
--   Term s PBool
-- pmatchDataRecHelperCred f cred1 cred2 = unTermCont $ do
--   x <- tcont $ pmatch cred1
--   y <- tcont $ pmatch cred2
--   pure $ case (x, y) of
--     (PPubKeyCredential a, PPubKeyCredential b) -> a `f` b
--     (PPubKeyCredential _, PScriptCredential _) -> pconstant True
--     (PScriptCredential _, PPubKeyCredential _) -> pconstant False
--     (PScriptCredential a, PScriptCredential b) -> a `f` b

-- Triplet utils

t1 :: Triplet Integer
t1 = Triplet 1 2 3

t2 :: Triplet Integer
t2 = Triplet 1 3 5

-- manual 'pmatch' + manual field extraction impl.
ltTrip :: Term s (PTriplet PInteger) -> Term s (PTriplet PInteger) -> Term s PBool
ltTrip trip1 trip2 = unTermCont $ do
  PTriplet x1 y1 z1 <- tcont $ pmatch trip1
  PTriplet x2 y2 z2 <- tcont $ pmatch trip2

  x <- tcont . plet . pfromData $ x1
  x' <- tcont . plet . pfromData $ x2
  pure $
    x
      #< x'
      #|| ( x
              #== x'
              #&& unTermCont
                ( do
                    y <- tcont . plet . pfromData $ y1
                    y' <- tcont . plet . pfromData $ y2
                    pure $ y #< y' #|| (y #== y' #&& pfromData z1 #< pfromData z2)
                )
          )

lteTrip :: Term s (PTriplet PInteger) -> Term s (PTriplet PInteger) -> Term s PBool
lteTrip trip1 trip2 = unTermCont $ do
  PTriplet x1 y1 z1 <- tcont $ pmatch trip1
  PTriplet x2 y2 z2 <- tcont $ pmatch trip2

  x <- tcont . plet . pfromData $ x1
  x' <- tcont . plet . pfromData $ x2
  pure $
    x
      #< x'
      #|| ( x
              #== x'
              #&& unTermCont
                ( do
                    y <- tcont . plet . pfromData $ y1
                    y' <- tcont . plet . pfromData $ y2
                    pure $ y #< y' #|| (y #== y' #&& pfromData z1 #<= pfromData z2)
                )
          )

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
