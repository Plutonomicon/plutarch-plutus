{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.POrdSpec (spec) where

import Data.ByteString (ByteString)

import Plutus.V1.Ledger.Api (
  Address (Address),
  Credential (PubKeyCredential, ScriptCredential),
  PubKeyHash (PubKeyHash),
  StakingCredential (StakingHash, StakingPtr),
  ValidatorHash (ValidatorHash),
 )
import qualified PlutusTx as PlutusTx
import qualified PlutusTx.Builtins as PlutusTx

import Test.QuickCheck.Instances ()
import Test.Syd
import Test.Tasty.QuickCheck (Arbitrary, arbitrary, oneof, property)

import Plutarch.Api.V1
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstant (PConstanted),
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Prelude
import Plutarch.SpecTypes (PTriplet)

spec :: Spec
spec = do
  describe "ord.pisdata" $ do
    propertySet @PBool "PBool"
    propertySet @(PMaybeData PInteger) "PMaybeData PInteger"
    propertySet @(PTriplet PInteger) "PMaybeData PInteger"
    propertySet @PAddress' "PAddress"

propertySet ::
  forall p.
  ( PIsData p
  , PLift p
  , POrd p
  , PlutusTx.ToData (PLifted p)
  , PlutusTx.FromData (PLifted p)
  , Ord (PLifted p)
  , Show (PLifted p)
  , Arbitrary (PLifted p)
  ) =>
  String ->
  Spec
propertySet typeName' = do
  describe typeName' $ do
    let typeName = '(' : typeName' ++ ")"
    specify ("(#<) @" <> typeName <> " ≡ (<) @" <> typeName) $
      property $ pltIso @p
    specify ("(#<=) @" <> typeName <> " ≡ (<=) @" <> typeName) $
      property $ plteIso @p
    specify ("(#==) @" <> typeName <> " ≡ (==) @" <> typeName) $
      property $ peqIso @p

pltIso :: forall p h. (p ~ PConstanted h, h ~ PLifted p, PConstant h, Arbitrary h, Ord h, POrd p) => h -> h -> IO ()
pltIso a b = plift (pconstant @p a #< pconstant b) `shouldBe` (a < b)

plteIso :: forall p h. (p ~ PConstanted h, h ~ PLifted p, PConstant h, Arbitrary h, Ord h, POrd p) => h -> h -> IO ()
plteIso a b = plift (pconstant @p a #<= pconstant b) `shouldBe` (a <= b)

peqIso :: forall p h. (p ~ PConstanted h, h ~ PLifted p, PConstant h, Arbitrary h, Eq h, PEq p) => h -> h -> IO ()
peqIso a b = plift (pconstant @p a #== pconstant b) `shouldBe` (a == b)

newtype PAddress' s = PAddress' (Term s PAddress)
  deriving (PlutusType, PIsData, PEq, POrd) via DerivePNewtype PAddress' PAddress

instance PUnsafeLiftDecl PAddress' where type PLifted PAddress' = Address'

newtype Address' = Address' Address
  deriving stock (Show, Eq, Ord)
  deriving newtype (PlutusTx.FromData, PlutusTx.ToData)
  deriving (PConstant) via (DerivePConstantViaNewtype Address' PAddress' PAddress)

instance Arbitrary Address' where
  arbitrary = Address' <$> arbitraryAddr
    where
      arbitraryAddr = Address <$> arbitraryCred <*> arbitraryMaybeStakingCred
      arbitraryCred =
        oneof
          [ PubKeyCredential . PubKeyHash . PlutusTx.toBuiltin @ByteString <$> arbitrary
          , ScriptCredential . ValidatorHash . PlutusTx.toBuiltin @ByteString <$> arbitrary
          ]
      arbitraryStakingCred =
        oneof
          [ StakingHash <$> arbitraryCred
          , StakingPtr <$> arbitrary <*> arbitrary <*> arbitrary
          ]
      arbitraryMaybeStakingCred = oneof [pure Nothing, Just <$> arbitraryStakingCred]
