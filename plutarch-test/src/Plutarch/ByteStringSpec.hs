module Plutarch.ByteStringSpec (spec) where

import Test.Syd

import qualified Data.ByteString as BS
import Plutarch.Prelude
import Plutarch.Test

spec :: Spec
spec = do
  describe "bytestring" $ do
    it "empty" $
      passert $ mempty #== phexByteStr ""
    describe "phexByteStr" $ do
      let a :: [String] = ["42", "ab", "df", "c9"]
          p = pconstant @PByteString (BS.pack $ map readByte a) #== phexByteStr (concat a)
      golden Bench p
      it "relation" $ passert p
    describe "plengthByteStr" $ do
      let p = (plengthBS # phexByteStr "012f") #== 2
      golden All p
      it "works" $ passert p
      describe "pconsBS" $ do
        let xs = phexByteStr "48fCd1"
            p = (plengthBS #$ pconsBS # 91 # xs) #== (1 + plengthBS # xs)
        golden All p
        it "works" $ passert p
    describe "pindexByteStr" $ do
      let p = (pindexBS # phexByteStr "4102af" # 1) #== pconstant @PInteger 0x02
      golden All p
      it "works" $ passert p
    describe "psliceByteStr" $ do
      let p = (psliceBS # 2 # 3 # phexByteStr "4102afde5b2a") #== phexByteStr "afde5b"
      golden All p
      it "works" $ passert p
    describe "eq" $ do
      let p = phexByteStr "12" #== phexByteStr "12"
      golden All p
      it "works" $ passert p
    describe "semigroup" $ do
      let s1 = phexByteStr "12"
          s2 = phexByteStr "34"
      golden All $ s1 <> s2
      it "laws" $ do
        passert $ (mempty <> s1) #== s1
        passert $ s1 #== (mempty <> s1)
      it "concats" $ do
        passert $ s1 <> s2 #== phexByteStr "1234"

{- | Interpret a byte.

>>> readByte "41"
65
-}
readByte :: Num a => String -> a
readByte a = fromInteger $ read $ "0x" <> a
