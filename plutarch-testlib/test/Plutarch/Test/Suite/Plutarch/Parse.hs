{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Test.Suite.Plutarch.Parse (tests) where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift (LiftError (CouldNotDecodeData, OtherLiftError))
import Plutarch.Prelude
import Plutarch.Repr.Tag (DeriveAsTag (DeriveAsTag))
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
  mkI,
  mkList,
  mkNilData,
  unitval,
  unsafeDataAsConstr,
  unsafeDataAsI,
  unsafeDataAsList,
 )
import PlutusTx.Builtins.Internal qualified as PTx
import Test.Tasty (TestTree, testGroup)
import "plutarch-testlib" Plutarch.Test.Golden (goldenEval, plutarchGolden)

tests :: TestTree
tests =
  testGroup
    "PValidateData"
    [ plutarchGolden
        "goldens"
        "pparseData"
        [ goldenEval "newtype" (pparseData @PANewtype $ pconstant @PData aNewtypeData)
        , goldenEval "record" (pparseData @PAProduct $ pconstant @PData aProductData)
        , goldenEval "sum" (pparseData @PASum $ pconstant @PData aSumData)
        , goldenEval "tag" (pparseData @PATag $ pconstant @PData aTagData)
        ]
    ]

-- Helpers

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

data ATag = TagOne | TagTwo | TagThree

instance ToData ATag where
  toBuiltinData = \case
    TagOne -> mkI 0
    TagTwo -> mkI 1
    TagThree -> mkI 2

instance FromData ATag where
  fromBuiltinData d = chooseData d Nothing Nothing Nothing go Nothing
    where
      go :: Maybe ATag
      go = do
        let i = unsafeDataAsI d
        if
          | i == 0 -> pure TagOne
          | i == 1 -> pure TagTwo
          | i == 2 -> pure TagThree
          | otherwise -> Nothing

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

data PATag (s :: S) = PTagOne | PTagTwo | PTagThree
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType, PValidateData) via (DeriveAsTag PATag)

instance PLiftable PATag where
  type AsHaskell PATag = ATag
  type PlutusRepr PATag = Integer
  haskToRepr = \case
    TagOne -> 0
    TagTwo -> 1
    TagThree -> 2
  reprToHask = \case
    0 -> pure TagOne
    1 -> pure TagTwo
    2 -> pure TagThree
    _ -> Left . OtherLiftError $ "Not a valid tag"
  plutToRepr = plutToReprUni
  reprToPlut = reprToPlutUni

aProductData :: Data
aProductData = List [aNewtypeData, I 42, I 42]

aSumData :: Data
aSumData = Constr 2 [aNewtypeData, I 42]

aTagData :: Data
aTagData = I 2
