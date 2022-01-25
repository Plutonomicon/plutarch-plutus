{-# LANGUAGE TemplateHaskell #-}

module Examples.LetRec (tests) where

import Plutarch (printTerm)
import Plutarch.Builtin (pasConstr, pforgetData)
import Plutarch.Prelude
import Plutarch.Rec (
  DataReader (DataReader, readData),
  DataWriter (DataWriter, writeData),
  PRecord (PRecord),
  RecordFromData,
  ScottEncoded,
  ScottEncoding,
  field,
  fieldFromData,
  letrec,
  rcon,
  recordDataFromFieldWriters,
  recordFromFieldReaders,
  rmatch,
 )
import Plutarch.Rec.TH (deriveAll)
import Plutarch.Unsafe (punsafeCoerce, punsafeFrom)
import qualified Rank2.TH
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Utils
import Prelude hiding (even, odd)

data FlatOuterRecord f = FlatOuterRecord
  { flatOuterBool :: f PBool
  , flatInner1 :: SampleRecord f
  , flatOuterInt :: f PInteger
  , flatInner2 :: SampleRecord f
  , flatOuterString :: f PString
  }

data ShallowOuterRecord f = ShallowOuterRecord
  { shallowOuterBool :: f PBool
  , shallowInner1 :: f (PRecord SampleRecord)
  , shallowOuterInt :: f PInteger
  , shallowInner2 :: f (PRecord SampleRecord)
  , shallowOuterString :: f PString
  }

data SampleRecord f = SampleRecord
  { sampleBool :: f PBool
  , sampleInt :: f PInteger
  , sampleString :: f PString
  }

data EvenOdd f = EvenOdd
  { even :: f (PInteger :--> PBool)
  , odd :: f (PInteger :--> PBool)
  }

type instance ScottEncoded EvenOdd a = (PInteger :--> PBool) :--> (PInteger :--> PBool) :--> a

$(Rank2.TH.deriveAll ''EvenOdd)
$(deriveAll ''SampleRecord) -- also autoderives the @type instance ScottEncoded@
$(deriveAll ''FlatOuterRecord)
$(deriveAll ''ShallowOuterRecord)
instance RecordFromData SampleRecord
instance RecordFromData FlatOuterRecord
instance RecordFromData ShallowOuterRecord

instance PIsData (PRecord SampleRecord) where
  pfromData = readData (recordFromFieldReaders sampleReader)
  pdata = writeData (recordDataFromFieldWriters sampleWriter)

instance PIsData (PRecord FlatOuterRecord) where
  pfromData = readData (recordFromFieldReaders flatOuterReader)
  pdata = writeData (recordDataFromFieldWriters flatOuterWriter)

instance PIsData (PRecord ShallowOuterRecord) where
  pfromData = readData (recordFromFieldReaders shallowOuterReader)
  pdata = writeData (recordDataFromFieldWriters shallowOuterWriter)

sampleReader :: SampleRecord (DataReader s)
sampleReader =
  SampleRecord
    { sampleBool = DataReader pfromData
    , sampleInt = DataReader pfromData
    , sampleString = DataReader $ \d -> pdecodeUtf8 #$ pfromData $ punsafeCoerce d
    }

sampleWriter :: SampleRecord (DataWriter s)
sampleWriter =
  SampleRecord
    { sampleBool = DataWriter pdata
    , sampleInt = DataWriter pdata
    , sampleString = DataWriter $ \s -> punsafeCoerce $ pdata $ pencodeUtf8 # s
    }

flatOuterReader :: FlatOuterRecord (DataReader s)
flatOuterReader =
  FlatOuterRecord
    { flatOuterBool = DataReader pfromData
    , flatInner1 = sampleReader
    , flatOuterInt = DataReader pfromData
    , flatInner2 = sampleReader
    , flatOuterString = DataReader $ \d -> pdecodeUtf8 #$ pfromData $ punsafeCoerce d
    }

flatOuterWriter :: FlatOuterRecord (DataWriter s)
flatOuterWriter =
  FlatOuterRecord
    { flatOuterBool = DataWriter pdata
    , flatInner1 = sampleWriter
    , flatOuterInt = DataWriter pdata
    , flatInner2 = sampleWriter
    , flatOuterString = DataWriter $ \s -> punsafeCoerce $ pdata $ pencodeUtf8 # s
    }

shallowOuterReader :: ShallowOuterRecord (DataReader s)
shallowOuterReader =
  ShallowOuterRecord
    { shallowOuterBool = DataReader pfromData
    , shallowInner1 = DataReader pfromData
    , shallowOuterInt = DataReader pfromData
    , shallowInner2 = DataReader pfromData
    , shallowOuterString = DataReader $ \d -> pdecodeUtf8 #$ pfromData $ punsafeCoerce d
    }

shallowOuterWriter :: ShallowOuterRecord (DataWriter s)
shallowOuterWriter =
  ShallowOuterRecord
    { shallowOuterBool = DataWriter pdata
    , shallowInner1 = DataWriter pdata
    , shallowOuterInt = DataWriter pdata
    , shallowInner2 = DataWriter pdata
    , shallowOuterString = DataWriter $ \s -> punsafeCoerce $ pdata $ pencodeUtf8 # s
    }

sampleFlatOuter :: Term (s :: S) (ScottEncoding FlatOuterRecord (t :: PType))
sampleFlatOuter = rcon rawFlatOuter

rawFlatOuter :: FlatOuterRecord (Term s)
rawFlatOuter =
  FlatOuterRecord
    { flatOuterBool = pcon PFalse
    , flatInner1 = rawRecord
    , flatOuterInt = 4
    , flatInner2 = rawRecord {sampleInt = 9}
    , flatOuterString = "Hola, Mundo!"
    }

sampleShallowOuter :: Term (s :: S) (ScottEncoding ShallowOuterRecord (t :: PType))
sampleShallowOuter = rcon rawShallowOuter

rawShallowOuter :: ShallowOuterRecord (Term s)
rawShallowOuter =
  ShallowOuterRecord
    { shallowOuterBool = pcon PFalse
    , shallowInner1 = pcon $ PRecord rawRecord
    , shallowOuterInt = 4
    , shallowInner2 = pcon $ PRecord rawRecord {sampleInt = 9}
    , shallowOuterString = "Hola, Mundo!"
    }

sampleRecord :: Term (s :: S) (ScottEncoding SampleRecord (t :: PType))
sampleRecord = rcon rawRecord

sampleRecord' :: Term (s :: S) (ScottEncoding SampleRecord (t :: PType))
sampleRecord' = pcon' $ PRecord rawRecord

sampleRecord'' :: Term (s :: S) (PRecord SampleRecord :: PType)
sampleRecord'' = pcon $ PRecord rawRecord

rawRecord :: SampleRecord (Term s)
rawRecord =
  SampleRecord
    { sampleBool = pcon PFalse
    , sampleInt = 6
    , sampleString = "Salut, Monde!"
    }

sampleRecur :: Term (s :: S) (ScottEncoding SampleRecord (t :: PType))
sampleRecur =
  letrec $
    const
      SampleRecord
        { sampleBool = pcon PTrue
        , sampleInt = 12
        , sampleString = "Hello, World!"
        }

evenOdd :: Term (s :: S) (ScottEncoding EvenOdd (t :: PType))
evenOdd = letrec evenOddRecursion
  where
    evenOddRecursion :: EvenOdd (Term s) -> EvenOdd (Term s)
    evenOddRecursion EvenOdd {even, odd} =
      EvenOdd
        { even = plam $ \n -> pif (n #== 0) (pcon PTrue) (odd #$ n - 1)
        , odd = plam $ \n -> pif (n #== 0) (pcon PFalse) (even #$ n - 1)
        }

sampleData :: Term s (PAsData (PRecord SampleRecord))
sampleData = pdata (punsafeFrom sampleRecord)

flatOuterData :: Term s (PAsData (PRecord FlatOuterRecord))
flatOuterData = pdata (punsafeFrom sampleFlatOuter)

shallowOuterData :: Term s (PAsData (PRecord ShallowOuterRecord))
shallowOuterData = pdata (punsafeFrom sampleShallowOuter)

tests :: HasTester => TestTree
tests =
  testGroup
    "Records"
    [ testGroup
        "Simple"
        [ testCase "record construction with pcon" $
            printTerm sampleRecord''
              @?= "(program 1.0.0 (\\i0 -> i1 False 6 \"Salut, Monde!\"))"
        , testCase "record construction with pcon'" $
            printTerm sampleRecord'
              @?= "(program 1.0.0 (\\i0 -> i1 False 6 \"Salut, Monde!\"))"
        , testCase "record construction with rcon" $
            printTerm sampleRecord
              @?= "(program 1.0.0 (\\i0 -> i1 False 6 \"Salut, Monde!\"))"
        , testCase "field access term" $
            printTerm (sampleRecord' # field sampleInt)
              @?= "(program 1.0.0 ((\\i0 -> i1 False 6 \"Salut, Monde!\") (\\i0 -> \\i0 -> \\i0 -> i2)))"
        , testGroup "field value" $
            [ testCase "direct access" $
                equal' (sampleRecord # field sampleInt) "(program 1.0.0 6)"
            , testCase "pmatch" $
                equal' (pmatch sampleRecord'' $ \(PRecord r) -> sampleString r) "(program 1.0.0 \"Salut, Monde!\")"
            , testCase "pmatch'" $
                equal' (pmatch' sampleRecord $ \(PRecord r) -> sampleString r) "(program 1.0.0 \"Salut, Monde!\")"
            , testCase "rmatch" $
                equal' (rmatch sampleRecord $ \SampleRecord {sampleString = s} -> s) "(program 1.0.0 \"Salut, Monde!\")"
            ]
        , testCase "record reconstruction with pcon" $
            printTerm (pmatch' sampleRecord' (pcon @(PRecord SampleRecord)))
              @?= "(program 1.0.0 ((\\i0 -> i1 False 6 \"Salut, Monde!\") (\\i0 -> \\i0 -> \\i0 -> \\i0 -> i1 i4 i3 i2)))"
        , testCase "reconstructed field access" $
            equal' (pto (pmatch' sampleRecord' (pcon @(PRecord SampleRecord))) # field sampleInt) "(program 1.0.0 6)"
        ]
    , testGroup
        "Letrec"
        [ testCase "record" $ (printTerm $ sampleRecur # field sampleInt) @?= "(program 1.0.0 ((\\i0 -> (\\i0 -> i2 (\\i0 -> i2 i2 i1)) (\\i0 -> i2 (\\i0 -> i2 i2 i1))) (\\i0 -> \\i0 -> i1 True 12 \"Hello, World!\") (\\i0 -> \\i0 -> \\i0 -> i2)))"
        , testCase "record field" $ equal' (sampleRecur # field sampleInt) "(program 1.0.0 12)"
        , testCase "even" $ (printTerm $ evenOdd # field even) @?= "(program 1.0.0 ((\\i0 -> (\\i0 -> (\\i0 -> (\\i0 -> i2 (\\i0 -> i2 i2 i1)) (\\i0 -> i2 (\\i0 -> i2 i2 i1))) (\\i0 -> \\i0 -> i1 (\\i0 -> force (i4 (equalsInteger i1 0) (delay True) (delay (i3 (\\i0 -> \\i0 -> i1) (subtractInteger i1 1))))) (\\i0 -> force (i4 (equalsInteger i1 0) (delay False) (delay (i3 i5 (subtractInteger i1 1)))))) i2) (force ifThenElse)) (\\i0 -> \\i0 -> i2)))"
        , testCase "even 4" $ equal' (evenOdd # field even # (4 :: Term s PInteger)) "(program 1.0.0 True)"
        , testCase "even 5" $ equal' (evenOdd # field even # (5 :: Term s PInteger)) "(program 1.0.0 False)"
        ]
    , testGroup
        "flat nested"
        [ testCase "record construction with rcon" $
            printTerm (sampleFlatOuter)
              @?= "(program 1.0.0 (\\i0 -> i1 False False 6 \"Salut, Monde!\" 4 False 9 \"Salut, Monde!\" \"Hola, Mundo!\"))"
        , testCase "nested field access" $
            printTerm (sampleFlatOuter # field (sampleInt . flatInner2))
              @?= "(program 1.0.0 ((\\i0 -> i1 False False 6 \"Salut, Monde!\" 4 False 9 \"Salut, Monde!\" \"Hola, Mundo!\") (\\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> i3)))"
        , testGroup "nested field value" $
            [ testCase "direct access" $
                equal' (sampleFlatOuter # field (sampleInt . flatInner2)) "(program 1.0.0 9)"
            , testCase "pmatch" $
                equal' (pmatch (pcon $ PRecord rawFlatOuter) $ \(PRecord r) -> sampleInt $ flatInner2 r) "(program 1.0.0 9)"
            , testCase "pmatch'" $
                equal' (pmatch' sampleFlatOuter $ \(PRecord r) -> sampleString $ flatInner2 r) "(program 1.0.0 \"Salut, Monde!\")"
            , testCase "rmatch" $
                equal' (rmatch sampleFlatOuter $ \FlatOuterRecord {flatInner2 = SampleRecord {sampleString = s}} -> s) "(program 1.0.0 \"Salut, Monde!\")"
            ]
        , testCase "reconstruct with pcon" $
            printTerm (pmatch' sampleFlatOuter (pcon @(PRecord FlatOuterRecord)))
              @?= "(program 1.0.0 ((\\i0 -> i1 False False 6 \"Salut, Monde!\" 4 False 9 \"Salut, Monde!\" \"Hola, Mundo!\") (\\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> i1 i10 i9 i8 i7 i6 i5 i4 i3 i2)))"
        , testCase "reconstruction nested field value" $
            equal' (pto (pmatch' sampleFlatOuter (pcon @(PRecord FlatOuterRecord))) # field (sampleInt . flatInner2)) "(program 1.0.0 9)"
        , testCase "nested record access term" $
            printTerm
              ( pmatch' (rcon rawFlatOuter) $
                  \(PRecord FlatOuterRecord {flatInner1}) -> pcon $ PRecord flatInner1
              )
              @?= "(program 1.0.0 ((\\i0 -> i1 False False 6 \"Salut, Monde!\" 4 False 9 \"Salut, Monde!\" \"Hola, Mundo!\") (\\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> i1 i9 i8 i7)))"
        , testCase "nested match term" $
            printTerm
              ( rmatch (rcon rawFlatOuter) $ \(FlatOuterRecord {flatInner2}) ->
                  rmatch (rcon flatInner2) $ \(SampleRecord {sampleString}) ->
                    sampleString
              )
              @?= "(program 1.0.0 ((\\i0 -> i1 False False 6 \"Salut, Monde!\" 4 False 9 \"Salut, Monde!\" \"Hola, Mundo!\") (\\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> (\\i0 -> i1 i5 i4 i3) (\\i0 -> \\i0 -> \\i0 -> i1))))"
        , testCase "nested match value" $
            equal'
              ( rmatch (rcon rawFlatOuter) $ \(FlatOuterRecord {flatInner2}) ->
                  rmatch (rcon flatInner2) $ \(SampleRecord {sampleString}) ->
                    sampleString
              )
              "(program 1.0.0 \"Salut, Monde!\")"
        ]
    , testGroup
        "shallow nested"
        [ testCase "record construction with rcon" $
            printTerm (sampleShallowOuter)
              @?= "(program 1.0.0 (\\i0 -> i1 False (\\i0 -> i1 False 6 \"Salut, Monde!\") 4 (\\i0 -> i1 False 9 \"Salut, Monde!\") \"Hola, Mundo!\"))"
        , testCase "nested field access" $
            printTerm (pto (sampleShallowOuter # field shallowInner2) # field sampleInt)
              @?= "(program 1.0.0 ((\\i0 -> i1 False (\\i0 -> i1 False 6 \"Salut, Monde!\") 4 (\\i0 -> i1 False 9 \"Salut, Monde!\") \"Hola, Mundo!\") (\\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> i2) (\\i0 -> \\i0 -> \\i0 -> i2)))"
        , testGroup "nested field value" $
            [ testCase "direct access" $
                equal' (pto (sampleShallowOuter # field shallowInner2) # field sampleInt) "(program 1.0.0 9)"
            , testCase "pmatch" $
                equal' (pmatch (pcon $ PRecord rawShallowOuter) $ \(PRecord r) -> pto (shallowInner2 r) # field sampleInt) "(program 1.0.0 9)"
            , testCase "pmatch'" $
                equal' (pmatch' sampleShallowOuter $ \(PRecord r) -> pto (shallowInner2 r) # field sampleString) "(program 1.0.0 \"Salut, Monde!\")"
            , testCase "rmatch" $
                equal' (rmatch sampleShallowOuter $ \ShallowOuterRecord {shallowInner2 = inner} -> pto inner # field sampleString) "(program 1.0.0 \"Salut, Monde!\")"
            ]
        , testCase "reconstruct with pcon" $
            printTerm (pmatch' sampleShallowOuter (pcon @(PRecord ShallowOuterRecord)))
              @?= "(program 1.0.0 ((\\i0 -> i1 False (\\i0 -> i1 False 6 \"Salut, Monde!\") 4 (\\i0 -> i1 False 9 \"Salut, Monde!\") \"Hola, Mundo!\") (\\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> i1 i6 i5 i4 i3 i2)))"
        , testCase "reconstruction nested field value" $
            equal' (pto (pto (pmatch' sampleShallowOuter (pcon @(PRecord ShallowOuterRecord))) # field shallowInner2) # field sampleInt) "(program 1.0.0 9)"
        , testCase "nested record access term" $
            printTerm (pmatch' sampleShallowOuter $ \(PRecord ShallowOuterRecord {shallowInner1}) -> shallowInner1)
              @?= "(program 1.0.0 ((\\i0 -> i1 False (\\i0 -> i1 False 6 \"Salut, Monde!\") 4 (\\i0 -> i1 False 9 \"Salut, Monde!\") \"Hola, Mundo!\") (\\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> i4)))"
        , testCase "nested match term" $
            printTerm
              ( pmatch' sampleShallowOuter $ \(PRecord ShallowOuterRecord {shallowInner2}) ->
                  pmatch shallowInner2 $ \(PRecord SampleRecord {sampleString}) ->
                    sampleString
              )
              @?= "(program 1.0.0 ((\\i0 -> i1 False (\\i0 -> i1 False 6 \"Salut, Monde!\") 4 (\\i0 -> i1 False 9 \"Salut, Monde!\") \"Hola, Mundo!\") (\\i0 -> \\i0 -> \\i0 -> \\i0 -> \\i0 -> i2 (\\i0 -> \\i0 -> \\i0 -> i1))))"
        , testCase "nested match value" $
            equal'
              ( pmatch' sampleShallowOuter $ \(PRecord ShallowOuterRecord {shallowInner2}) ->
                  pmatch shallowInner2 $ \(PRecord SampleRecord {sampleString}) ->
                    sampleString
              )
              "(program 1.0.0 \"Salut, Monde!\")"
        ]
    , testGroup
        "Data"
        [ testGroup
            "pdata"
            [ testCase "simple" $ printTerm sampleData @?= "(program 1.0.0 ((\\i0 -> i1 False 6 \"Salut, Monde!\") (\\i0 -> \\i0 -> \\i0 -> constrData 0 (force mkCons ((\\i0 -> constrData (force ifThenElse i1 1 0) [  ]) i3) (force mkCons (iData i2) (force mkCons (bData (encodeUtf8 i1)) [  ]))))))"
            , testCase "simple value deconstructed" $ equal' (pasConstr # pforgetData sampleData) "(program 1.0.0 (0, [#d87980, #06, #4d53616c75742c204d6f6e646521]))"
            , testCase "flat data deconstructed" $
                equal'
                  (pasConstr # pforgetData flatOuterData)
                  "(program 1.0.0 ( 0\n, [ #d87980\n  , #d87980\n  , #06\n  , #4d53616c75742c204d6f6e646521\n  , #04\n  , #d87980\n  , #09\n  , #4d53616c75742c204d6f6e646521\n  , #4c486f6c612c204d756e646f21 ] ))"
            , testCase "shallow data deconstructed" $
                equal'
                  (pasConstr # pforgetData shallowOuterData)
                  "(program 1.0.0 ( 0\n, [ #d87980\n  , #d8799fd87980064d53616c75742c204d6f6e646521ff\n  , #04\n  , #d8799fd87980094d53616c75742c204d6f6e646521ff\n  , #4c486f6c612c204d756e646f21 ] ))"
            ]
        , testGroup
            "fieldFromData term"
            [ testCase "simple record" $ (printTerm $ plam $ \dat -> plam pfromData #$ fieldFromData sampleInt # dat) @?= "(program 1.0.0 (\\i0 -> unIData ((\\i0 -> (\\i0 -> force (force ifThenElse (equalsInteger (force (force fstPair) i1) 0) (delay (force headList (force tailList (force (force sndPair) i1)))) (delay error))) (unConstrData i1)) i1)))"
            , testCase "flat nested" $ (printTerm $ plam $ \dat -> plam pfromData #$ fieldFromData (sampleInt . flatInner2) # dat) @?= "(program 1.0.0 ((\\i0 -> \\i0 -> unIData ((\\i0 -> (\\i0 -> force (force ifThenElse (equalsInteger (force (force fstPair) i1) 0) (delay (force headList (i4 (i4 (i4 (i4 (i4 (i4 (force (force sndPair) i1))))))))) (delay error))) (unConstrData i1)) i1)) (force tailList)))"
            , testCase "shallow nested" $ (printTerm $ plam $ \dat -> pto (plam pfromData #$ fieldFromData shallowInner2 # dat) # field sampleInt) @?= "(program 1.0.0 ((\\i0 -> (\\i0 -> (\\i0 -> (\\i0 -> (\\i0 -> \\i0 -> (\\i0 -> (\\i0 -> force (i4 (equalsInteger (i5 i1) 0) (delay (\\i0 -> i1 ((\\i0 -> equalsInteger (i7 (unConstrData i1)) 1) (i7 (i9 i2))) (unIData (i7 (i8 (i9 i2)))) (decodeUtf8 (unBData (i7 (i8 (i8 (i9 i2)))))))) (delay error))) (unConstrData i1)) ((\\i0 -> (\\i0 -> force (i4 (equalsInteger (i5 i1) 0) (delay (i6 (i7 (i7 (i7 (i8 i1)))))) (delay error))) (unConstrData i1)) i1) (\\i0 -> \\i0 -> \\i0 -> i2)) (force ifThenElse)) (force (force fstPair))) (force headList)) (force tailList)) (force (force sndPair))))"
            ]
        , testGroup
            "fieldFromData value"
            [ testCase "simple" $ equal' (fieldFromData sampleInt # sampleData) "(program 1.0.0 #06)"
            , testCase "flat nested" $ equal' (fieldFromData (sampleInt . flatInner2) # flatOuterData) "(program 1.0.0 #09)"
            , testCase "shallow nested" $ equal' (fieldFromData sampleInt #$ fieldFromData shallowInner2 #$ shallowOuterData) "(program 1.0.0 #09)"
            ]
        , testCase "pfromData term" $ (printTerm $ plam $ \d -> punsafeCoerce (pfromData d :: Term _ (PRecord SampleRecord)) # field sampleInt) @?= "(program 1.0.0 ((\\i0 -> (\\i0 -> (\\i0 -> (\\i0 -> \\i0 -> (\\i0 -> force (force ifThenElse (equalsInteger (i3 i1) 0) (delay (\\i0 -> i1 ((\\i0 -> equalsInteger (i5 (unConstrData i1)) 1) (i5 (i7 i2))) (unIData (i5 (i6 (i7 i2)))) (decodeUtf8 (unBData (i5 (i6 (i6 (i7 i2)))))))) (delay error))) (unConstrData i1) (\\i0 -> \\i0 -> \\i0 -> i2)) (force (force fstPair))) (force headList)) (force tailList)) (force (force sndPair))))"
        ]
    ]
