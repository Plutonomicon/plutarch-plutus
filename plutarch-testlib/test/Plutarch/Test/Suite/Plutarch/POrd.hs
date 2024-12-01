{-# LANGUAGE AllowAmbiguousTypes #-}

module Plutarch.Test.Suite.Plutarch.POrd (tests) where

import Data.Kind (Type)
import GHC.Records (getField)
import Plutarch.LedgerApi.V1 (
  PAddress,
  PCredential (PPubKeyCredential, PScriptCredential),
  PMaybeData,
  PPubKeyHash (PPubKeyHash),
  PScriptHash (PScriptHash),
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
                [ goldenGroup "derived" (ltWith @PCredential (#<) c1 c2)
                , goldenGroup "pmatch" (ltWith ltCred c1 c2)
                , goldenGroup "pmatch-pdatarecord" (ltWith ltCred' c1 c2)
                ]
            , goldenGroup
                "PTriplet"
                [ goldenGroup "derived" (ltWith @(PTriplet PInteger) (#<) t1 t2)
                , goldenGroup "pmatch" (ltWith ltTrip t1 t2)
                , goldenGroup "pmatch-pdatarecord" (ltWith ltTrip' t1 t2)
                ]
            ]
        , plutarchGolden
            "lte"
            "pisdata.lte"
            [ goldenGroup
                "PCredential"
                [ goldenGroup "derived" (lteWith @PCredential (#<=) c1 c2)
                , goldenGroup "pmatch" (lteWith lteCred c1 c2)
                , goldenGroup "pmatch-pdatarecord" (lteWith lteCred' c1 c2)
                ]
            , goldenGroup
                "PTriplet"
                [ goldenGroup "derived" (lteWith @(PTriplet PInteger) (#<=) t1 t2)
                , goldenGroup "pmatch" (lteWith lteTrip t1 t2)
                , goldenGroup "pmatch-pdatarecord" (lteWith lteTrip' t1 t2)
                ]
            ]
        ]
    , testGroup
        "Haskell Equivalence"
        [ checkHaskellOrdEquivalent @PBool
        , checkHaskellOrdEquivalent @(PMaybeData PInteger)
        , checkHaskellOrdEquivalent @(PTriplet PInteger)
        , checkHaskellOrdEquivalent @PAddress
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

ltCred' :: Term s PCredential -> Term s PCredential -> Term s PBool
ltCred' = pmatchDataRecHelperCred (#<)

lteCred' :: Term s PCredential -> Term s PCredential -> Term s PBool
lteCred' = pmatchDataRecHelperCred (#<=)

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
    (PPubKeyCredential a, PPubKeyCredential b) ->
      let a' = pfromData $ pfield @"_0" # a
          b' = pfromData $ pfield @"_0" # b
       in pmatch a' $ \(PPubKeyHash a'') ->
            pmatch b' $ \(PPubKeyHash b'') ->
              pmatch a'' $ \(PDataNewtype a''') ->
                pmatch b'' $ \(PDataNewtype b''') ->
                  f (pfromData a''') (pfromData b''')
    (PPubKeyCredential _, PScriptCredential _) -> pconstant True
    (PScriptCredential _, PPubKeyCredential _) -> pconstant False
    (PScriptCredential a, PScriptCredential b) ->
      let a' = pfield @"_0" # a
          b' = pfield @"_0" # b
       in pmatch a' $ \(PScriptHash a'') ->
            pmatch b' $ \(PScriptHash b'') ->
              pmatch a'' $ \(PDataNewtype a''') ->
                pmatch b'' $ \(PDataNewtype b''') ->
                  f (pfromData a''') (pfromData b''')

-- manual 'pmatch' + 'PDataRecord' Ord impl.
pmatchDataRecHelperCred ::
  (forall l. POrd (PDataRecord l) => Term s (PDataRecord l) -> Term s (PDataRecord l) -> Term s PBool) ->
  Term s PCredential ->
  Term s PCredential ->
  Term s PBool
pmatchDataRecHelperCred f cred1 cred2 = unTermCont $ do
  x <- tcont $ pmatch cred1
  y <- tcont $ pmatch cred2
  pure $ case (x, y) of
    (PPubKeyCredential a, PPubKeyCredential b) -> a `f` b
    (PPubKeyCredential _, PScriptCredential _) -> pconstant True
    (PScriptCredential _, PPubKeyCredential _) -> pconstant False
    (PScriptCredential a, PScriptCredential b) -> a `f` b

-- Triplet utils

t1 :: Triplet Integer
t1 = Triplet 1 2 3

t2 :: Triplet Integer
t2 = Triplet 1 3 5

-- manual 'pmatch' + manual field extraction impl.
ltTrip :: Term s (PTriplet PInteger) -> Term s (PTriplet PInteger) -> Term s PBool
ltTrip trip1 trip2 = unTermCont $ do
  a <- tcont $ pletFields @'["x", "y", "z"] trip1
  b <- tcont $ pletFields @'["x", "y", "z"] trip2

  x <- tcont . plet . pfromData $ getField @"x" a
  x' <- tcont . plet . pfromData $ getField @"x" b
  pure $
    x
      #< x'
      #|| ( x
              #== x'
              #&& unTermCont
                ( do
                    y <- tcont . plet . pfromData $ getField @"y" a
                    y' <- tcont . plet . pfromData $ getField @"y" b
                    pure $ y #< y' #|| (y #== y' #&& pfromData (getField @"z" a) #< pfromData (getField @"z" b))
                )
          )

ltTrip' :: Term s (PTriplet PInteger) -> Term s (PTriplet PInteger) -> Term s PBool
ltTrip' = pmatchDataRecHelperTrip (#<)

lteTrip :: Term s (PTriplet PInteger) -> Term s (PTriplet PInteger) -> Term s PBool
lteTrip trip1 trip2 = unTermCont $ do
  a <- tcont $ pletFields @'["x", "y", "z"] trip1
  b <- tcont $ pletFields @'["x", "y", "z"] trip2

  x <- tcont . plet . pfromData $ getField @"x" a
  x' <- tcont . plet . pfromData $ getField @"x" b
  pure $
    x
      #< x'
      #|| ( x
              #== x'
              #&& unTermCont
                ( do
                    y <- tcont . plet . pfromData $ getField @"y" a
                    y' <- tcont . plet . pfromData $ getField @"y" b
                    pure $ y #< y' #|| (y #== y' #&& pfromData (getField @"z" a) #<= pfromData (getField @"z" b))
                )
          )

lteTrip' :: Term s (PTriplet PInteger) -> Term s (PTriplet PInteger) -> Term s PBool
lteTrip' = pmatchDataRecHelperTrip (#<=)

-- manual 'pmatch' + 'PDataRecord' Ord impl.
pmatchDataRecHelperTrip ::
  (forall l. POrd (PDataRecord l) => Term s (PDataRecord l) -> Term s (PDataRecord l) -> Term s PBool) ->
  Term s (PTriplet PInteger) ->
  Term s (PTriplet PInteger) ->
  Term s PBool
pmatchDataRecHelperTrip f trip1 trip2 = unTermCont $ do
  PTriplet a <- tcont $ pmatch trip1
  PTriplet b <- tcont $ pmatch trip2
  pure $ a `f` b

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
