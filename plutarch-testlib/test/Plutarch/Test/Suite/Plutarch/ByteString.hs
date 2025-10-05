module Plutarch.Test.Suite.Plutarch.ByteString (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import GHC.Exts (fromList)
import Plutarch.Prelude
import Plutarch.String (pisHexDigit)
import Plutarch.Test.Golden (goldenEval, goldenGroup, plutarchGolden)
import Plutarch.Test.Unit (testEvalEqual)
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
        , goldenEval "plengthByteStr" ((plengthBS # phexByteStr "012f") #== 2)
        , goldenEval
            "pconsBS"
            ( let xs = phexByteStr "48fCd1"
               in (plengthBS #$ pconsBS # pconstant 91 # xs) #== (1 + plengthBS # xs)
            )
        , goldenEval "pindexByteStr" (pindexBS # phexByteStr "4102af" # 1)
        , goldenEval "psliceByteStr" (psliceBS # 2 # 3 # phexByteStr "4102afde5b2a")
        , goldenEval "eq" (phexByteStr "12" #== phexByteStr "12")
        , let s1 = phexByteStr "12"
              s2 = phexByteStr "34"
           in goldenGroup
                "semigroup"
                [ goldenEval "concats" (s1 <> s2)
                , goldenGroup
                    "laws"
                    [ goldenEval "id.1" ((mempty <> s1) #== s1)
                    , goldenEval "id.2" (s1 #== (mempty <> s1))
                    ]
                ]
        ]
    , testGroup
        "Unit tests"
        [ testGroup
            "pisHexDigit"
            [ testEvalEqual
                "numbers are hex digits"
                (pallBS # plam (\x -> pisHexDigit #$ pbyteToInteger # x) # pconstant "0123456789")
                (pcon PTrue)
            , testEvalEqual
                "A-F are hex digits"
                (pallBS # plam (\x -> pisHexDigit #$ pbyteToInteger # x) # pconstant "ABCDEF")
                (pcon PTrue)
            , testEvalEqual
                "a-f are hex digits"
                (pallBS # plam (\x -> pisHexDigit #$ pbyteToInteger # x) # pconstant "abcdef")
                (pcon PTrue)
            , testEvalEqual
                "no other ASCII code is a hex digit"
                (pallBS # plam (\x -> pnot #$ pisHexDigit #$ pbyteToInteger # x) # pconstant nonHexAscii)
                (pcon PTrue)
            ]
        ]
    ]

readByte :: Num a => String -> a
readByte a = fromInteger $ read $ "0x" <> a

nonHexAscii :: ByteString
nonHexAscii =
  -- All codes up to, but not including, the first digit
  fromList [0, 1 .. 47]
    <>
    -- Between digits to upper-case
    fromList [58, 59 .. 64]
    <>
    -- Between upper-case and lower-case
    fromList [71, 72 .. 96]
    <>
    -- After lower-case
    fromList [103 .. 127]

pallBS ::
  forall (s :: S).
  Term s ((PByte :--> PBool) :--> PByteString :--> PBool)
pallBS = phoistAcyclic $ plam $ \p bs ->
  plet (plengthBS # bs) $ \len ->
    go p len bs # pconstant 0
  where
    go ::
      forall (s' :: S).
      Term s' (PByte :--> PBool) ->
      Term s' PInteger ->
      Term s' PByteString ->
      Term s' (PInteger :--> PBool)
    go p len bs = pfixHoisted #$ plam $ \self ix ->
      pif
        (ix #< len)
        ( pif
            (p #$ pindexBS # bs # ix)
            (self # (ix + 1))
            (pcon PFalse)
        )
        (pcon PTrue)
