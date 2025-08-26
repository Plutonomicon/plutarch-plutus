{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift (LiftError (CouldNotDecodeData))
import Plutarch.Internal.Parse (PValidateData (pwithValidated), pparseData)
import Plutarch.Internal.Term (
  Config (NoTracing, Tracing),
  LogLevel (LogInfo),
  TracingMode (DoTracing),
  punsafeCoerce,
 )
import Plutarch.LedgerApi.Utils (
  PMaybeData,
  pmapMaybeData,
  pmaybeDataToMaybe,
  pmaybeToMaybeData,
 )
import Plutarch.Maybe (pmapMaybe)
import Plutarch.Prelude
import Plutarch.Repr.Data (
  DeriveAsDataRec (DeriveAsDataRec),
  DeriveAsDataStruct (DeriveAsDataStruct),
 )
import Plutarch.Test.Bench (
  BenchConfig (NonOptimizing, Optimizing),
  bcompare,
  bench,
  benchWithConfig,
  defaultMain,
 )
import Plutarch.Test.Suite.Plutarch.Unroll (unrollBenches)
import Plutarch.Test.Utils (precompileTerm)
import PlutusCore.Data (Data (B, Constr, I, List))
import PlutusTx (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  fromData,
  toData,
 )
import PlutusTx.Builtins.HasBuiltin (fromBuiltin, toBuiltin)
import PlutusTx.Builtins.Internal (
  BuiltinByteString,
  BuiltinData,
  caseList',
  chooseData,
  mkCons,
  mkConstr,
  mkList,
  mkNilData,
  unitval,
  unsafeDataAsConstr,
  unsafeDataAsList,
 )
import PlutusTx.Builtins.Internal qualified as PTx
import Test.Tasty (TestTree, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Benchmarks"
      [ testGroup "Maybe" maybeBenches
      , testGroup "Exponentiation" expBenches
      , testGroup "Tracing" tracingBenches
      , testGroup "Unroll" unrollBenches
      , testGroup "PValidateData" pvalidateDataBenches
      ]

-- Suites

pvalidateDataBenches :: [TestTree]
pvalidateDataBenches =
  [ bench "Newtype" (precompileTerm (plam $ pparseData @PANewtype) # pconstant @PData aNewtypeData)
  , bcompare "$(NF-1) == \"PValidateData\" && $NF == \"Newtype\"" $
      bench "PTryFrom newtype" (precompileTerm (plam $ \x -> ptryFrom @(PAsData PANewtype) x fst) # pconstant @PData aNewtypeData)
  , bench "Record" (precompileTerm (plam $ pparseData @PAProduct) # pconstant @PData aProductData)
  , bcompare "$(NF-1) == \"PValidateData\" && $NF == \"Record\"" $
      bench "PTryFrom record" (precompileTerm (plam $ \x -> ptryFrom @(PAsData PAProduct) x fst) # pconstant @PData aProductData)
  , bench "Sum" (precompileTerm (plam $ pparseData @PASum) # pconstant @PData aSumData)
  , bcompare "$(NF-1) == \"PValidateData\" && $NF == \"Sum\"" $
      bench "PTryFrom sum" (precompileTerm (plam $ \x -> ptryFrom @(PAsData PASum) x fst) # pconstant @PData aSumData)
  ]

tracingBenches :: [TestTree]
tracingBenches =
  [ benchWithConfig "with tracing" traceConfig t
  , bcompare "$(NF-1) == \"Tracing\" && $NF == \"with tracing\"" $
      benchWithConfig "no tracing" noTraceConfig t
  ]
  where
    traceConfig :: BenchConfig
    traceConfig = NonOptimizing $ Tracing LogInfo DoTracing
    noTraceConfig :: BenchConfig
    noTraceConfig = NonOptimizing NoTracing
    t :: forall (s :: S). Term s PBool
    t = ptraceInfo "I am a very long string which hopefully uses a lot of space" (pconstant True)

expBenches :: [TestTree]
expBenches =
  [ bench "linear" (linearExp # 3 # 31)
  , bcompare "$(NF-1) == \"Exponentiation\" && $NF == \"linear\"" $ bench "by squaring" (bySquaringExp # 3 # 31)
  ]

maybeBenches :: [TestTree]
maybeBenches =
  [ testGroup
      "pmaybeToMaybeData . pmaybeDataToMaybe"
      [ bench
          "non-optimized"
          (plam (\m -> pmaybeToMaybeData #$ pmaybeDataToMaybe # m) # pconstant @(PMaybeData PInteger) (Just 42))
      , bcompare
          "$(NF-1) == \"pmaybeToMaybeData . pmaybeDataToMaybe\" && $NF == \"non-optimized\""
          $ benchWithConfig
            "optimized"
            Optimizing
            (plam (\m -> pmaybeToMaybeData #$ pmaybeDataToMaybe # m) # pconstant @(PMaybeData PInteger) (Just 42))
      ]
  , testGroup
      "fmap even"
      [ bench
          "PMaybeData"
          (pmapMaybeData # plam (\v -> pdata (peven # pfromData v)) # pconstant @(PMaybeData PInteger) (Just 42))
      , bcompare "$(NF-1) == \"fmap even\" && $NF == \"PMaybeData\"" $
          bench "PMaybe vs PMaybeData" (pmapMaybe # peven # pconstant @(PMaybe PInteger) (Just 42))
      ]
  , -- We run both cheap and expensive calculation in 'pmap*' to mitigate impact of PAsData encoding/decoding
    let
      n :: Integer = 10
     in
      testGroup
        "fmap fib"
        [ bench
            "PMaybeData"
            (pmapMaybeData # plam (\v -> pdata (pfib # pfromData v)) # pconstant @(PMaybeData PInteger) (Just n))
        , bcompare "$(NF-1) == \"fmap fib\" && $NF == \"PMaybeData\"" $
            bench "PMaybe vs PMaybeData" (pmapMaybe # pfib # pconstant @(PMaybe PInteger) (Just n))
        ]
  ]

-- Helpers

peven :: Term s (PInteger :--> PBool)
peven = plam $ \n -> pmod # n # 2 #== 0

pfib :: Term s (PInteger :--> PInteger)
pfib = pfix #$ plam $ \self n -> pif (n #<= 1) (pconstant 1) ((self # (n - 1)) * (self # (n - 2)))

linearExp :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
linearExp = phoistAcyclic $ plam $ \b e ->
  inner # b # b # e
  where
    inner :: forall (s' :: S). Term s' (PInteger :--> PInteger :--> PInteger :--> PInteger)
    inner = phoistAcyclic $ pfix #$ plam $ \self b acc e ->
      pif
        (e #== pone)
        acc
        (self # b # (acc #* b) # (e #- pone))

bySquaringExp :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
bySquaringExp = phoistAcyclic $ pfix #$ plam $ \self b e ->
  pif
    (e #== pone)
    b
    ( plet (self # b #$ pquot # e # 2) $ \below ->
        plet (below #* below) $ \res ->
          pif
            ((prem # e # 2) #== pone)
            (b #* res)
            res
    )

-- Data

newtype ANewtype = ANewtype ByteString

data ASum
  = One ByteString
  | Other Integer
  | Both ByteString Integer

instance ToData ASum where
  toBuiltinData = \case
    One bs -> mkConstr 0 . mkCons (toBuiltinData . toBuiltin $ bs) . mkNilData $ unitval
    Other i -> mkConstr 1 . mkCons (toBuiltinData i) . mkNilData $ unitval
    Both bs i -> mkConstr 2 . mkCons (toBuiltinData . toBuiltin $ bs) . mkCons (toBuiltinData i) . mkNilData $ unitval

instance FromData ASum where
  fromBuiltinData d = chooseData d go Nothing Nothing Nothing Nothing
    where
      go :: Maybe ASum
      go = do
        let p = unsafeDataAsConstr d
        let ix = PTx.fst p
        let fields = PTx.snd p
        if
          | ix == 0 -> caseList' Nothing (\h _ -> case0 h) fields
          | ix == 1 -> caseList' Nothing (\h _ -> case1 h) fields
          | ix == 2 -> caseList' Nothing (\h1 t -> caseList' Nothing (\h2 _ -> case2 h1 h2) t) fields
          | otherwise -> Nothing
      case0 :: BuiltinData -> Maybe ASum
      case0 dat = One . fromBuiltin @BuiltinByteString <$> fromBuiltinData dat
      case1 :: BuiltinData -> Maybe ASum
      case1 dat = Other <$> fromBuiltinData dat
      case2 :: BuiltinData -> BuiltinData -> Maybe ASum
      case2 dat1 dat2 = Both <$> (fromBuiltin @BuiltinByteString <$> fromBuiltinData dat1) <*> fromBuiltinData dat2

data AProduct = AProduct ByteString Integer Integer

instance ToData AProduct where
  toBuiltinData (AProduct f1 f2 f3) =
    mkList
      ( mkCons (toBuiltinData . toBuiltin $ f1)
          . mkCons (toBuiltinData f2)
          . mkCons (toBuiltinData f3)
          . mkNilData
          $ unitval
      )

instance FromData AProduct where
  fromBuiltinData d = chooseData d Nothing Nothing go Nothing Nothing
    where
      go :: Maybe AProduct
      go = do
        let ell = unsafeDataAsList d
        caseList'
          Nothing
          ( \h1 t1 ->
              caseList'
                Nothing
                ( \h2 t2 ->
                    caseList'
                      Nothing
                      ( \h3 _ -> do
                          f1 <- fromBuiltinData h1
                          f2 <- fromBuiltinData h2
                          f3 <- fromBuiltinData h3
                          pure . AProduct (fromBuiltin @BuiltinByteString f1) f2 $ f3
                      )
                      t2
                )
                t1
          )
          ell

newtype PANewtype (s :: S) = PANewtype (Term s PByteString)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving
    ( PlutusType
    )
    via (DeriveNewtypePlutusType PANewtype)

instance PLiftable PANewtype where
  type AsHaskell PANewtype = ANewtype
  type PlutusRepr PANewtype = ByteString
  haskToRepr = coerce
  reprToHask = Right . ANewtype
  reprToPlut = reprToPlutUni
  plutToRepr = plutToReprUni

instance PTryFrom PData (PAsData PANewtype) where
  ptryFrom' opq = runTermCont $ do
    _ <- tcont $ plet (pasByteStr # opq)
    pure (punsafeCoerce opq, ())

instance PValidateData PANewtype where
  pwithValidated opq x = plet (pasByteStr # opq) $ const x

aNewtypeData :: Data
aNewtypeData = B "deadbeefdeadbeefdeadbeef"

data PASum (s :: S)
  = POne (Term s (PAsData PByteString))
  | POther (Term s (PAsData PInteger))
  | PBoth (Term s (PAsData PByteString)) (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType, PValidateData) via (DeriveAsDataStruct PASum)

instance PLiftable PASum where
  type AsHaskell PASum = ASum
  type PlutusRepr PASum = Data
  haskToRepr = toData
  reprToHask = maybe (Left CouldNotDecodeData) Right . fromData
  plutToRepr = plutToReprUni
  reprToPlut = reprToPlutUni

instance PTryFrom PData (PAsData PASum) where
  ptryFrom' opq = runTermCont $ do
    p <- tcont $ plet (pasConstr # opq)
    ix <- tcont $ plet (pfstBuiltin # p)
    fields <- tcont $ plet (psndBuiltin # p)
    pure
      ( pif
          (ix #== 0)
          ( unTermCont $ do
              _ <- tcont $ ptryFrom @(PAsData PByteString) (pheadBuiltin # fields)
              pure . punsafeCoerce $ opq
          )
          ( pif
              (ix #== 1)
              ( unTermCont $ do
                  _ <- tcont $ ptryFrom @(PAsData PInteger) (pheadBuiltin # fields)
                  pure . punsafeCoerce $ opq
              )
              ( pif
                  (ix #== 2)
                  ( unTermCont $ do
                      _ <- tcont $ ptryFrom @(PAsData PByteString) (pheadBuiltin # fields)
                      _ <- tcont $ ptryFrom @(PAsData PInteger) (pheadBuiltin #$ ptailBuiltin # fields)
                      pure . punsafeCoerce $ opq
                  )
                  perror
              )
          )
      , ()
      )

data PAProduct (s :: S)
  = PAProduct
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType, PValidateData) via (DeriveAsDataRec PAProduct)

instance PLiftable PAProduct where
  type AsHaskell PAProduct = AProduct
  type PlutusRepr PAProduct = Data
  haskToRepr = toData
  reprToHask x = case fromData x of
    Nothing -> Left CouldNotDecodeData
    Just y -> pure y
  plutToRepr = plutToReprUni
  reprToPlut = reprToPlutUni

instance PTryFrom PData (PAsData PAProduct) where
  ptryFrom' opq = runTermCont $ do
    ell <- tcont $ plet (pasList # opq)
    _ <- tcont $ ptryFrom @(PAsData PByteString) (pheadBuiltin # ell)
    t1 <- tcont $ plet (ptailBuiltin # ell)
    _ <- tcont $ ptryFrom @(PAsData PInteger) (pheadBuiltin # t1)
    _ <- tcont $ ptryFrom @(PAsData PInteger) (pheadBuiltin #$ ptailBuiltin # t1)
    pure (punsafeCoerce opq, ())

aProductData :: Data
aProductData = List [aNewtypeData, I 42, I 42]

aSumData :: Data
aSumData = Constr 2 [aNewtypeData, I 42]
