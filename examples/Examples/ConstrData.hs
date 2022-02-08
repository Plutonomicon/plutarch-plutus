{-# LANGUAGE UndecidableInstances #-}

module Examples.ConstrData (tests) where

import Data.String (fromString)
import qualified GHC.Generics as GHC
import Generics.SOP

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Utils

import Plutarch.Api.V1
import Plutarch.DataRepr (PDataFields, PIsDataReprInstances (PIsDataReprInstances))
import Plutarch.Lift (PConstanted, PLifted)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Plutus.V1.Ledger.Api
import qualified PlutusTx

{- |
  We can defined a data-type using PDataRecord, with labeled fields.

  With an appropriate instance of 'PIsDataRepr', we can automatically
  derive 'PDataFields'.
-}
newtype Triplet (a :: PType) (s :: S)
  = Triplet
      ( Term
          s
          ( PDataRecord
              '[ "x" ':= a
               , "y" ':= a
               , "z" ':= a
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via (PIsDataReprInstances (Triplet a))

data PVehicle (s :: S)
  = PFourWheeler (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PInteger, "_2" ':= PInteger, "_3" ':= PInteger]))
  | PTwoWheeler (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PInteger]))
  | PImmovableBox (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PVehicle

data PEnumType (s :: S)
  = PA (Term s (PDataRecord '[]))
  | PB (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PEnumType

tests :: HasTester => TestTree
tests =
  testGroup
    "Data construction tests"
    [ testCase "Sum of products construction" $ do
        pcon
          ( PFourWheeler $
              pdcons
                # pconstantData 2 #$ pdcons
                # pconstantData 5 #$ pdcons
                # pconstantData 42 #$ pdcons
                # pconstantData 0
                # pdnil
          )
          `equal` punsafeCoerce
            (pconstant $ PlutusTx.Constr 0 [PlutusTx.I 2, PlutusTx.I 5, PlutusTx.I 42, PlutusTx.I 0])
        pcon (PTwoWheeler $ pdcons # pconstantData 5 #$ pdcons # pconstantData 0 # pdnil)
          `equal` punsafeCoerce
            (pconstant $ PlutusTx.Constr 1 [PlutusTx.I 5, PlutusTx.I 0])
        pcon (PImmovableBox pdnil)
          `equal` punsafeCoerce
            (pconstant $ PlutusTx.Constr 2 [])
    , testCase "Product construction" $ do
        pcon
          ( Triplet $
              pdcons
                # pconstantData @PCurrencySymbol "ab" #$ pdcons
                # pconstantData "41" #$ pdcons
                # pconstantData "0e"
                # pdnil
          )
          `equal` punsafeCoerce
            ( pconstant $
                PlutusTx.Constr
                  0
                  [ PlutusTx.toData @CurrencySymbol "ab"
                  , PlutusTx.toData @CurrencySymbol "41"
                  , PlutusTx.toData @CurrencySymbol "0e"
                  ]
            )
        let minting = Minting ""
            spending = Spending $ TxOutRef "ab" 0
            rewarding = Rewarding . StakingHash $ PubKeyCredential "da"
        pcon
          ( Triplet $
              pdcons
                # pconstantData minting #$ pdcons
                # pconstantData spending #$ pdcons
                # pconstantData rewarding
                # pdnil
          )
          `equal` punsafeCoerce
            ( pconstant $
                PlutusTx.Constr
                  0
                  [PlutusTx.toData minting, PlutusTx.toData spending, PlutusTx.toData rewarding]
            )
    , testCase "Enumerable sum type construction" $ do
        pcon (PA pdnil) `equal` punsafeCoerce (pconstant $ PlutusTx.Constr 0 [])
        pcon (PB pdnil) `equal` punsafeCoerce (pconstant $ PlutusTx.Constr 1 [])
    , testCase "Relation between pconstant and pcon" $ do
        let valHash = "01"
            addr = Address (ScriptCredential $ fromString valHash) Nothing
            pscriptCredential :: Term s PCredential
            pscriptCredential =
              pcon $
                PScriptCredential $
                  pdcons # pdata (pcon $ PValidatorHash $ phexByteStr valHash) # pdnil
        pconstant addr
          `equal` pcon (PAddress $ pdcons # pdata pscriptCredential #$ pdcons # pdata (pcon $ PDNothing pdnil) # pdnil)
    ]
