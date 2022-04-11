{-# LANGUAGE TemplateHaskell #-}

module Plutarch.RecSpec (spec) where

import qualified Rank2.TH

import Prelude hiding (even, odd)

import Plutarch (pcon', pmatch')
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
import Plutarch.Test
import Plutarch.Unsafe (punsafeCoerce, punsafeFrom)
import Test.Hspec

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

spec :: Spec
spec = do
  -- Plutarch.Rec.verifySoleConstructor uses tracing, so we must create two sets
  -- of golden.
  describe "rec" . plutarchDevFlagDescribe . pgoldenSpec $ do
    "simple" @\ do
      -- Record construction
      "constr" @\ do
        "pcon" @| sampleRecord''
        "pcon'" @| sampleRecord'
        "rcon" @| sampleRecord
      "field" @\ do
        "access-term" @| sampleRecord' # field sampleInt
        "value" @\ do
          "direct-access" @| sampleRecord # field sampleInt
          "pmatch" @| pmatch sampleRecord'' $ \(PRecord r) -> sampleString r
          "pmatch'" @| pmatch' sampleRecord $ \(PRecord r) -> sampleString r
          "rmatch" @| rmatch sampleRecord $ \SampleRecord {sampleString = s} -> s
      -- Record reconstruction
      "reconstr" @\ do
        "pcon" @| pmatch' sampleRecord' (pcon @(PRecord SampleRecord))
        -- reconstructed field access
        "field-access" @| pto (pmatch' sampleRecord' (pcon @(PRecord SampleRecord))) # field sampleInt
    "LetRec" @\ do
      "record" @| sampleRecur # field sampleInt
      "record-field" @| sampleRecur # field sampleInt
      "even" @| evenOdd # field even
      "even.4" @| evenOdd # field even # (4 :: Term s PInteger)
      "even.5" @| evenOdd # field even # (5 :: Term s PInteger)
    "nested" @\ do
      "flat" @\ do
        "reconstr-with-rcon" @| sampleFlatOuter
        "nested-field-access" @| sampleFlatOuter # field (sampleInt . flatInner2)
        "nested-field-value" @\ do
          "direct-access" @| sampleFlatOuter # field (sampleInt . flatInner2)
          "pmatch" @| pmatch (pcon $ PRecord rawFlatOuter) $ \(PRecord r) -> sampleInt $ flatInner2 r
          "pmatch'" @| pmatch' sampleFlatOuter $ \(PRecord r) -> sampleString $ flatInner2 r
          "rmatch" @| rmatch sampleFlatOuter $ \FlatOuterRecord {flatInner2 = SampleRecord {sampleString = s}} -> s
        "reconstr-with-pcon" @| pmatch' sampleFlatOuter (pcon @(PRecord FlatOuterRecord))
        "reconstr-nested-field-value"
          @| pto (pmatch' sampleFlatOuter (pcon @(PRecord FlatOuterRecord))) # field (sampleInt . flatInner2)
        "nested-record-access-term"
          @| pmatch' (rcon rawFlatOuter)
          $ \(PRecord FlatOuterRecord {flatInner1}) -> pcon $ PRecord flatInner1
        "nested-match-term"
          @| rmatch (rcon rawFlatOuter)
          $ \(FlatOuterRecord {flatInner2}) ->
            rmatch (rcon flatInner2) $ \(SampleRecord {sampleString}) ->
              sampleString
        "nested-match-value"
          @| rmatch (rcon rawFlatOuter)
          $ \(FlatOuterRecord {flatInner2}) ->
            rmatch (rcon flatInner2) $ \(SampleRecord {sampleString}) ->
              sampleString
      "shallow" @\ do
        "constr-with-rcon" @| sampleShallowOuter
        "nested-field-access"
          @| pto (sampleShallowOuter # field shallowInner2) # field sampleInt
        "nested-field-value" @\ do
          "direct-access"
            @| pto (sampleShallowOuter # field shallowInner2) # field sampleInt
          "pmatch"
            @| pmatch (pcon $ PRecord rawShallowOuter)
            $ \(PRecord r) -> pto (shallowInner2 r) # field sampleInt
          "pmatch'"
            @| pmatch' sampleShallowOuter
            $ \(PRecord r) -> pto (shallowInner2 r) # field sampleString
          "rmatch"
            @| rmatch sampleShallowOuter
            $ \ShallowOuterRecord {shallowInner2 = inner} -> pto inner # field sampleString
        "reconstr-with-pcon"
          @| pmatch' sampleShallowOuter (pcon @(PRecord ShallowOuterRecord))
        "reconstr-nested-field-value"
          @| pto (pto (pmatch' sampleShallowOuter (pcon @(PRecord ShallowOuterRecord))) # field shallowInner2) # field sampleInt
        "nested-record-access-term"
          @| pmatch' sampleShallowOuter
          $ \(PRecord ShallowOuterRecord {shallowInner1}) -> shallowInner1
        "nested-match-term"
          @| pmatch' sampleShallowOuter
          $ \(PRecord ShallowOuterRecord {shallowInner2}) ->
            pmatch shallowInner2 $ \(PRecord SampleRecord {sampleString}) ->
              sampleString
        "nested-match-value"
          @| pmatch' sampleShallowOuter
          $ \(PRecord ShallowOuterRecord {shallowInner2}) ->
            pmatch shallowInner2 $ \(PRecord SampleRecord {sampleString}) ->
              sampleString
    "Data" @\ do
      "pdata" @\ do
        "simple" @| sampleData
        "simple-value-deconstructed" @| pasConstr # pforgetData sampleData
        "flat-data-deconstructed" @| pasConstr # pforgetData flatOuterData
        "shallow-data-deconstructed" @| pasConstr # pforgetData shallowOuterData
      "fieldFromData" @\ do
        "simple" @| plam $ \dat -> plam pfromData #$ fieldFromData sampleInt # dat
        "nested.flat" @| plam $ \dat -> plam pfromData #$ fieldFromData (sampleInt . flatInner2) # dat
        "nested.shallow" @| plam $ \dat -> pto (plam pfromData #$ fieldFromData shallowInner2 # dat) # field sampleInt
      "pfromData" @| plam $ \d -> punsafeCoerce (pfromData d :: Term _ (PRecord SampleRecord)) # field sampleInt
