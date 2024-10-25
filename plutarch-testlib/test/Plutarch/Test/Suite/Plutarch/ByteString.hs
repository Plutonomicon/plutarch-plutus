module Plutarch.Test.Suite.Plutarch.ByteString (tests) where

import Data.ByteString qualified as BS
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenAssertEqual, goldenEval, goldenGroup, plutarchGolden)
import Test.Tasty (TestTree, testGroup)

{-# HLINT ignore tests "Monoid law, left identity" #-}
tests :: TestTree
tests =
  testGroup
    "ByteString"
    [ plutarchGolden
        "Goldens"
        "bytestring"
        [ goldenEval "empty" (mempty #== phexByteStr "")
        , goldenEval
            "phexByteStr"
            ( let a :: [String] = ["42", "ab", "df", "c9"]
               in pconstant @PByteString (BS.pack $ fmap readByte a) #== phexByteStr (concat a)
            )
        , goldenAssertEqual "plengthByteStr" ((plengthBS # phexByteStr "012f") #== 2) (pcon PTrue)
        , goldenEval
            "pconsBS"
            ( let xs = phexByteStr "48fCd1"
               in (plengthBS #$ pconsBS # pconstant 91 # xs) #== (1 + plengthBS # xs)
            )
        , goldenAssertEqual
            "pindexByteStr"
            (pindexBS # phexByteStr "4102af" # 1)
            (pconstant @PByte 0x02)
        , goldenAssertEqual
            "psliceByteStr"
            (psliceBS # 2 # 3 # phexByteStr "4102afde5b2a")
            (phexByteStr "afde5b")
        , goldenEval "eq" (phexByteStr "12" #== phexByteStr "12")
        , let s1 = phexByteStr "12"
              s2 = phexByteStr "34"
           in goldenGroup
                "semigroup"
                [ goldenAssertEqual "concats" (s1 <> s2) (phexByteStr "1234")
                , goldenGroup
                    "laws"
                    [ goldenEval "id.1" ((mempty <> s1) #== s1)
                    , goldenEval "id.2" (s1 #== (mempty <> s1))
                    ]
                ]
        ]
    ]

readByte :: Num a => String -> a
readByte a = fromInteger $ read $ "0x" <> a
