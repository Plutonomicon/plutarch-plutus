{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Plutarch.FFISpec (spec) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch (compile, printScript, printTerm)
import Plutarch.Api.V1 (PCurrencySymbol, PPubKeyHash, PScriptContext, PTokenName, PTxInfo)
import Plutarch.Evaluate (EvalError, evalScript)
import Plutarch.FFI (
  PTxList,
  PTxMaybe (PTxJust, PTxNothing),
  foreignExport,
  foreignImport,
  opaqueExport,
  opaqueImport,
  pmaybeFromTx,
  pmaybeToTx,
 )
import Plutarch.List (pconvertLists)
import Plutarch.Prelude
import Plutarch.Rec qualified as Rec
import Plutarch.Rec.TH (deriveAll)
import Plutus.V1.Ledger.Api (
  Address (Address),
  Credential (ScriptCredential),
  CurrencySymbol,
  DatumHash,
  PubKeyHash (..),
  ScriptContext (ScriptContext),
  ScriptPurpose (Spending),
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  TxInfo (
    TxInfo,
    txInfoDCert,
    txInfoData,
    txInfoFee,
    txInfoId,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoSignatories,
    txInfoValidRange,
    txInfoWdrl
  ),
  TxOut (TxOut, txOutAddress, txOutDatumHash, txOutValue),
  TxOutRef (TxOutRef),
  ValidatorHash,
  Value,
  adaSymbol,
  adaToken,
  getTxId,
 )
import Plutus.V1.Ledger.Contexts qualified as Contexts
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx (CompiledCode, applyCode)
import PlutusTx qualified
import PlutusTx.Builtins.Internal (BuiltinBool, BuiltinUnit)
import PlutusTx.Prelude
import Shrink (shrinkScript, shrinkScriptSp, withoutTactics)

import Test.Hspec
import Test.Tasty.HUnit ((@?=))

-- import Test.Tasty.Plutus.Internal.Context (ContextBuilder (cbSignatories), TransactionConfig(..), compileSpending)
import Prelude (String)
import Prelude qualified

printCode :: CompiledCode a -> String
printCode = printScript . fromCompiledCode

printShrunkCode :: CompiledCode a -> String
printShrunkCode = printScript . shrink . shrink . shrink . fromCompiledCode
  where
    shrink = shrinkScriptSp (withoutTactics ["strongUnsubs", "weakUnsubs"])

printEvaluatedCode :: CompiledCode a -> Either EvalError String
printEvaluatedCode = fmap printScript . fstOf3 . evalScript . fromCompiledCode

printShrunkTerm :: ClosedTerm a -> String
printShrunkTerm x = printScript $ shrinkScript $ compile x

printEvaluatedTerm :: ClosedTerm a -> Either EvalError String
printEvaluatedTerm s = fmap printScript . fstOf3 . evalScript $ compile s

fstOf3 :: (a, _, _) -> a
fstOf3 (x, _, _) = x

doubleInPlutusTx :: CompiledCode (Integer -> Integer)
doubleInPlutusTx = $$(PlutusTx.compile [||(2 *) :: Integer -> Integer||])

doubleInPlutarch :: Term s (PInteger :--> PInteger)
doubleInPlutarch = plam (2 Prelude.*)

doubleImported :: Term s (PInteger :--> PInteger)
doubleImported = foreignImport doubleInPlutusTx

doubleExported :: PlutusTx.CompiledCode (Integer -> Integer)
doubleExported = foreignExport doubleInPlutarch

data SampleRecord = SampleRecord
  { sampleBool :: BuiltinBool
  , sampleInt :: Integer
  , sampleString :: BuiltinString
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)

data PSampleRecord f = PSampleRecord
  { psampleBool :: f PBool
  , psampleInt :: f PInteger
  , psampleString :: f PString
  }
$(deriveAll ''PSampleRecord)

data PSampleRecord' (s :: S) = PSampleRecord'
  { psampleBool' :: Term s PBool
  , psampleInt' :: Term s PInteger
  , psampleString' :: Term s PString
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PlutusType)

importedField :: Term _ (PDelayed (Rec.PRecord PSampleRecord) :--> PInteger)
importedField = foreignImport ($$(PlutusTx.compile [||sampleInt||]) :: CompiledCode (SampleRecord -> Integer))

exportedField :: CompiledCode (SampleRecord -> Integer)
exportedField =
  foreignExport
    ( (plam $ \r -> pmatch (pforce r) $ \(Rec.PRecord rr) -> psampleInt rr) ::
        Term _ (PDelayed (Rec.PRecord PSampleRecord) :--> PInteger)
    )

getTxInfo :: Term _ (PAsData PScriptContext :--> PAsData PTxInfo)
getTxInfo = pfield @"txInfo"

exportedTxInfo :: CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData)
exportedTxInfo = foreignExport getTxInfo

importedTxSignedBy :: Term _ (PAsData PTxInfo :--> PAsData PPubKeyHash :--> PBool)
importedTxSignedBy = foreignImport $$(PlutusTx.compile [||txDataSignedBy||])
  where
    txDataSignedBy :: BuiltinData -> BuiltinData -> BuiltinBool
    txDataSignedBy tx pkh = toBuiltin $ any id (Contexts.txSignedBy <$> PlutusTx.fromBuiltinData tx <*> PlutusTx.fromBuiltinData pkh)

importedTxSignedBy' :: Term _ (PAsData PTxInfo :--> PPubKeyHash :--> PBool)
importedTxSignedBy' = foreignImport $$(PlutusTx.compile [||txDataSignedBy||])
  where
    txDataSignedBy :: BuiltinData -> PubKeyHash -> BuiltinBool
    txDataSignedBy tx pkh = toBuiltin $ any id (flip Contexts.txSignedBy pkh <$> PlutusTx.fromBuiltinData tx)

type PSValue = PSMap PCurrencySymbol (PSMap PTokenName PInteger)

type PSMap k v = PTxList (PDelayed (PPair k v))

sumValueAmounts :: Term _ (PSValue :--> PInteger)
sumValueAmounts =
  pfoldl
    # (plam $ \s vals -> pfoldl # (plam $ \s' p -> s' Prelude.+ psnd # p) # s # (psnd # vals))
    # 0

pfst :: Term s (PDelayed (PPair a b) :--> a)
pfst = plam $ \p -> pmatch (pforce p) $ \(PPair x _) -> x

psnd :: Term s (PDelayed (PPair a b) :--> b)
psnd = plam $ \p -> pmatch (pforce p) $ \(PPair _ y) -> y

---- lifted from https://github.com/Plutonomicon/plutarch/blob/master/examples/Examples/Api.hs ----

{- |
  An example 'PScriptContext' Term,
  lifted with 'pconstant'
-}
ctx :: ScriptContext
ctx = ScriptContext info purpose

-- | Simple script context, with minting and a single input
info :: TxInfo
info =
  TxInfo
    { txInfoInputs = [inp]
    , txInfoOutputs = []
    , txInfoFee = mempty
    , txInfoMint = val
    , txInfoDCert = []
    , txInfoWdrl = []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = signatories
    , txInfoData = []
    , txInfoId = "b0"
    }

-- | A script input
inp :: TxInInfo
inp =
  TxInInfo
    { txInInfoOutRef = ref
    , txInInfoResolved =
        TxOut
          { txOutAddress =
              Address (ScriptCredential validator) Nothing
          , txOutValue = mempty
          , txOutDatumHash = Just datum
          }
    }

val :: Value
val = Value.singleton sym "sometoken" 1 <> Value.singleton adaSymbol adaToken 2

ref :: TxOutRef
ref = TxOutRef "a0" 0

purpose :: ScriptPurpose
purpose = Spending ref

validator :: ValidatorHash
validator = "a1"

datum :: DatumHash
datum = "d0"

sym :: CurrencySymbol
sym = "c0"

signatories :: [PubKeyHash]
signatories = ["ab01fe235c", "123014", "abcdef"]

{- | Project wide tests

 @since 0.1
-}
spec :: Spec
spec = describe "FFI" $ do
  describe "Simple types" $ do
    it "integer literal" $
      printCode $$(PlutusTx.compile [||42 :: Integer||]) @?= "(program 1.0.0 42)"
    it "PlutusTx integer function" $
      printCode doubleInPlutusTx @?= "(program 1.0.0 (\\i0 -> multiplyInteger 2 i1))"
    it "Plutarch integer function" $
      printTerm (plam $ \(x :: Term _ PInteger) -> 2 Prelude.* x) @?= "(program 1.0.0 (\\i0 -> multiplyInteger 2 i1))"
    it "Imported PlutusTx integer function" $
      printTerm doubleImported @?= "(program 1.0.0 (\\i0 -> multiplyInteger 2 i1))"
    it "Exported Plutarch integer function" $
      printCode doubleExported @?= "(program 1.0.0 (\\i0 -> multiplyInteger 2 i1))"
    it "Imported and applied PlutusTx integer function" $
      printTerm (plam $ \n -> doubleImported #$ doubleImported # n)
        @?= "(program 1.0.0 (\\i0 -> (\\i0 -> multiplyInteger 2 i1) (multiplyInteger 2 i1)))"
    it "Exported and applied Plutarch integer function" $
      printCode (doubleExported `applyCode` PlutusTx.liftCode (21 :: Integer))
        @?= "(program 1.0.0 ((\\i0 -> multiplyInteger 2 i1) 21))"
    it "Bool->Integer in Plutarch" $
      printShrunkTerm (plam $ \x -> pif x (1 :: Term _ PInteger) 0)
        @?= "(program 1.0.0 (\\i0 -> force (force ifThenElse i1 (delay 1) (delay 0))))"
    it "Bool->Integer in PlutusTx" $
      printShrunkCode $$(PlutusTx.compile [||\x -> if x then 1 :: Integer else 0||])
        @?= "(program 1.0.0 (\\i0 -> force i1 1 0))"
    it "newtype in PlutusTx" $
      printShrunkCode $$(PlutusTx.compile [||PubKeyHash||]) @?= "(program 1.0.0 (\\i0 -> i1))"
    it "export unit to PlutusTx" $
      printShrunkCode (foreignExport (pconstant ()) :: CompiledCode BuiltinUnit) @?= "(program 1.0.0 ())"
    it "import unit from PlutusTx" $
      printShrunkTerm (foreignImport $$(PlutusTx.compile [||toBuiltin ()||]) :: ClosedTerm PUnit) @?= "(program 1.0.0 ())"
  describe "Opaque" $ do
    it "Export an integer and ignore it" $
      printCode ($$(PlutusTx.compile [||const (7 :: Integer)||]) `applyCode` opaqueExport (4 :: ClosedTerm PInteger))
        @?= "(program 1.0.0 ((\\i0 -> 7) 4))"
    it "Import an integer and ignore it" $
      printTerm (plam (\_ -> 4 :: ClosedTerm PInteger) # opaqueImport (PlutusTx.liftCode (7 :: Integer)))
        @?= "(program 1.0.0 ((\\i0 -> 4) 7))"
  describe "Records" $ do
    it "PlutusTx record value" $
      printShrunkCode $$(PlutusTx.compile [||SampleRecord (toBuiltin False) 6 "Hello"||]) @?= sampleScottEncoding
    it "Plutarch HKD record value" $
      printTerm (pdelay $ Rec.rcon $ PSampleRecord (pcon PFalse) 6 "Hello") @?= sampleScottEncoding
    it "Plutarch SOP record value" $
      printTerm (pdelay $ pcon $ PSampleRecord' (pcon PFalse) 6 "Hello") @?= sampleScottEncoding
    it "PlutusTx record function" $
      printShrunkCode $$(PlutusTx.compile [||sampleInt||]) @?= sampleScottField
    it "Plutarch record function" $
      printTerm (plam $ \r -> pforce r # Rec.field psampleInt) @?= sampleScottField
    it "Apply PlutusTx record function in Plutarch" $
      printShrunkTerm (importedField #$ pdelay $ pcon $ Rec.PRecord $ PSampleRecord (pcon PFalse) 6 "Hello") @?= "(program 1.0.0 6)"
    it "Apply Plutarch record function in PlutusTx" $
      printShrunkCode (exportedField `applyCode` $$(PlutusTx.compile [||SampleRecord (toBuiltin False) 6 "Hello"||]))
        @?= "(program 1.0.0 6)"
    it "import a pair" $
      printEvaluatedTerm
        ( foreignImport (PlutusTx.liftCode ("foo" :: BuiltinString, 4 :: Integer)) ::
            Term _ (PDelayed (PPair PString PInteger))
        )
        @?= Right "(program 1.0.0 (delay (\\i0 -> i1 \"foo\" 4)))"
    it "import a pair" $
      printEvaluatedTerm
        ( foreignImport (PlutusTx.liftCode ("foo" :: Value.TokenName, 4 :: Integer)) ::
            Term _ (PDelayed (PPair PTokenName PInteger))
        )
        @?= Right "(program 1.0.0 (delay (\\i0 -> i1 #666f6f 4)))"
  describe "Maybe" $ do
    it "a PlutusTx Just Integer" $
      printShrunkCode (PlutusTx.liftCode (Just 4 :: Maybe Integer)) @?= justFour
    it "a converted Plutarch PJust PInteger" $
      printShrunkTerm (pmaybeToTx # (pcon (PJust 4) :: Term _ (PMaybe PInteger))) @?= justFour
    it "a Plutarch PTxJust PInteger" $
      printTerm (pcon (PTxJust 4) :: Term _ (PTxMaybe PInteger)) @?= justFour
    it "a converted Plutarch PTxJust PInteger" $
      printShrunkTerm (pmaybeFromTx # (pcon $ PTxJust 4) :: Term _ (PMaybe PInteger)) @?= "(program 1.0.0 (\\i0 -> \\i0 -> i2 4))"
    it "a PlutusTx Nothing" $
      printShrunkCode (PlutusTx.liftCode (Nothing :: Maybe Integer)) @?= nothing
    it "a converted Plutarch PNothing" $
      printShrunkTerm (pmaybeToTx # (pcon PNothing :: Term _ (PMaybe PInteger))) @?= nothing
    it "a Plutarch PTxNothing" $
      printTerm (pcon PTxNothing :: Term _ (PTxMaybe PInteger)) @?= nothing
    it "a converted Plutarch PTxNothing" $
      printShrunkTerm (pmaybeFromTx # (pcon PTxNothing) :: Term _ (PMaybe PInteger)) @?= "(program 1.0.0 (\\i0 -> \\i0 -> force i1))"
    it "import a Just Integer" $
      printEvaluatedTerm (foreignImport (PlutusTx.liftCode (Just 4 :: Maybe Integer)) :: Term _ (PTxMaybe PInteger)) @?= Right justFour
    it "export a PTxJust PInteger" $
      printEvaluatedCode
        ( (foreignExport (pcon (PTxJust 4) :: Term _ (PTxMaybe PInteger))) ::
            CompiledCode (Maybe Integer)
        )
        @?= Right justFour
  describe "Lists" $ do
    it "a PlutusTx list of integers" $
      printShrunkCode (PlutusTx.liftCode [1 :: Integer .. 3]) @?= oneTwoThree
    it "import a list of integers" $
      printEvaluatedTerm (foreignImport (PlutusTx.liftCode [1 :: Integer .. 3]) :: Term _ (PTxList PInteger)) @?= Right oneTwoThree
    it "import and map over a Value" $
      printEvaluatedTerm (pmap # pfst # (foreignImport (PlutusTx.liftCode val) :: Term _ PSValue))
        @?= Right "(program 1.0.0 (delay (\\i0 -> \\i0 -> i1 #c0 (delay (\\i0 -> \\i0 -> i1 # (delay (\\i0 -> \\i0 -> i2)))))))"
    it "import and fold over a Value" $
      printEvaluatedTerm
        (sumValueAmounts # (foreignImport (PlutusTx.liftCode val) :: Term _ PSValue))
        @?= Right "(program 1.0.0 3)"
    it "export a list of integers" $
      printEvaluatedCode
        ( foreignExport (pconvertLists #$ pconstant @(PBuiltinList PInteger) [1 .. 3] :: Term _ (PTxList PInteger)) ::
            CompiledCode [Integer]
        )
        @?= Right oneTwoThree
    it "export a fold and apply it to a Value" $
      printEvaluatedCode
        ((foreignExport sumValueAmounts :: CompiledCode (Value -> Integer)) `PlutusTx.applyCode` PlutusTx.liftCode val)
        @?= Right "(program 1.0.0 3)"

  describe "Data" $ do
    describe "Export and use a PData :--> PData function" $ do
      it "evaluate a field" $
        printEvaluatedCode
          ( $$(PlutusTx.compile [||\gti ctx -> maybe "undecoded" (getTxId . txInfoId) (PlutusTx.fromBuiltinData (gti ctx))||])
              `applyCode` exportedTxInfo
              `applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData ctx)
          )
          @?= Right "(program 1.0.0 #b0)"
      it "evaluate a function to True" $
        printEvaluatedCode
          ( $$(PlutusTx.compile [||\gti ctx pkh -> any (`Contexts.txSignedBy` pkh) (PlutusTx.fromBuiltinData (gti ctx))||])
              `applyCode` exportedTxInfo
              `applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData ctx)
              `applyCode` PlutusTx.liftCode (head signatories)
          )
          @?= Right "(program 1.0.0 (delay (\\i0 -> \\i0 -> i2)))"
      it "evaluate a function to False" $
        printEvaluatedCode
          ( $$(PlutusTx.compile [||\gti ctx pkh -> any (`Contexts.txSignedBy` pkh) (PlutusTx.fromBuiltinData (gti ctx))||])
              `applyCode` exportedTxInfo
              `applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData ctx)
              `applyCode` PlutusTx.liftCode "0123"
          )
          @?= Right "(program 1.0.0 (delay (\\i0 -> \\i0 -> i1)))"

    describe "Import and use a BuiltinData -> x function" $ do
      it "evaluate a Data -> Data -> Bool function to True" $
        printEvaluatedTerm (importedTxSignedBy # pconstantData info # pconstantData (head signatories))
          @?= Right "(program 1.0.0 True)"
      it "evaluate a Data -> Data -> Bool function to False" $
        printEvaluatedTerm (importedTxSignedBy # pconstantData info # pconstantData "0123")
          @?= Right "(program 1.0.0 False)"
      it "evaluate a Data -> PubKeyHash -> Bool function to True" $
        printEvaluatedTerm (importedTxSignedBy' # pconstantData info # pconstant (head signatories))
          @?= Right "(program 1.0.0 True)"
      it "evaluate a Data -> PubKeyHash -> Bool function to False" $
        printEvaluatedTerm (importedTxSignedBy' # pconstantData info # pconstant "0123")
          @?= Right "(program 1.0.0 False)"
  where
    sampleScottEncoding = "(program 1.0.0 (delay (\\i0 -> i1 False 6 \"Hello\")))"
    sampleScottField = "(program 1.0.0 (\\i0 -> force i1 (\\i0 -> \\i0 -> \\i0 -> i2)))"
    oneTwoThree, justFour :: String
    oneTwoThree = "(program 1.0.0 (delay (\\i0 -> \\i0 -> i1 1 (delay (\\i0 -> \\i0 -> i1 2 (delay (\\i0 -> \\i0 -> i1 3 (delay (\\i0 -> \\i0 -> i2)))))))))"
    justFour = "(program 1.0.0 (delay (\\i0 -> \\i0 -> i2 4)))"
    nothing = "(program 1.0.0 (delay (\\i0 -> \\i0 -> i1)))"
