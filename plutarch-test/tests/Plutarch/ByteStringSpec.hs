module Plutarch.ByteStringSpec (spec) where

import Data.ByteString qualified as BS
import Plutarch.Prelude
import Plutarch.Test
import Test.Hspec

{-# HLINT ignore spec "Monoid law, left identity" #-}
spec :: Spec
spec = do
  describe "bytestring" . pgoldenSpec $ do
    "empty" @| mempty #== phexByteStr "" @-> passert
    "phexByteStr"
      @| ( let a :: [String] = ["42", "ab", "df", "c9"]
            in pconstant @PByteString (BS.pack $ fmap readByte a) #== phexByteStr (concat a)
         )
      @-> passert
    "plengthByteStr" @| (plengthBS # phexByteStr "012f") #== 2 @-> passert
    "pconsBS"
      @| ( let xs = phexByteStr "48fCd1"
            in (plengthBS #$ pconsBS # 91 # xs) #== (1 + plengthBS # xs)
         )
      @-> passert
    "pindexByteStr"
      @| (pindexBS # phexByteStr "4102af" # 1)
      @== pconstant @PInteger 0x02
    "psliceByteStr"
      @| (psliceBS # 2 # 3 # phexByteStr "4102afde5b2a")
      @== phexByteStr "afde5b"
    "eq" @| phexByteStr "12" #== phexByteStr "12" @-> passert
    let s1 = phexByteStr "12"
        s2 = phexByteStr "34"
    "semigroup" @\ do
      "concats" @| s1 <> s2 @== phexByteStr "1234"
      "laws" @\ do
        "id.1" @| (mempty <> s1) #== s1 @-> passert
        "id.2" @| s1 #== (mempty <> s1) @-> passert

{- | Interpret a byte.

>>> readByte "41"
65
-}
readByte :: Num a => String -> a
readByte a = fromInteger $ read $ "0x" <> a
