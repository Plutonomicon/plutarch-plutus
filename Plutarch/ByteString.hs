module Plutarch.ByteString (
  PByteString,
  phexByteStr,
  pbyteStr,
  pconsBS,
  psliceBS,
  plengthBS,
  pindexBS,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Word (Word8)
import GHC.Stack (HasCallStack)
import Plutarch (punsafeBuiltin, punsafeConstant)
import Plutarch.Bool (PEq, POrd, (#<), (#<=), (#==))
import Plutarch.Integer (PInteger)
import Plutarch.Prelude
import qualified PlutusCore as PLC

-- | Plutus 'BuiltinByteString'
data PByteString s

instance PEq PByteString where
  x #== y = punsafeBuiltin PLC.EqualsByteString # x # y

instance POrd PByteString where
  x #<= y = punsafeBuiltin PLC.LessThanEqualsByteString # x # y
  x #< y = punsafeBuiltin PLC.LessThanByteString # x # y

instance Semigroup (Term s PByteString) where
  x <> y = punsafeBuiltin PLC.AppendByteString # x # y

instance Monoid (Term s PByteString) where
  mempty = punsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniByteString BS.empty

-- | Interpret a hex string as a PByteString.
phexByteStr :: HasCallStack => String -> Term s PByteString
phexByteStr = punsafeConstant . PLC.Some . PLC.ValueOf PLC.DefaultUniByteString . BS.pack . f
  where
    f "" = []
    f [_] = error "UnevenLength"
    f (x : y : rest) = (hexDigitToWord8 x * 16 + hexDigitToWord8 y) : f rest

-- | Construct a PByteString term from a Haskell bytestring.
pbyteStr :: ByteString -> Term s PByteString
pbyteStr = punsafeConstant . PLC.Some . PLC.ValueOf PLC.DefaultUniByteString

-----------------------------------------------------------
-- The following functions should be import qualified. --
-----------------------------------------------------------

-- | Prepend a byte, represented by a non negative 'PInteger', to a 'PBytestring'.
pconsBS :: Term s (PInteger :--> PByteString :--> PByteString)
pconsBS = punsafeBuiltin PLC.ConsByteString

{- | Slice a 'PByteString' with given start and end indices.

>>> (pslice # 1 # 3 phexByteStr "4102afde5b2a") #== phexByteStr "02afde"
-}
psliceBS :: Term s (PInteger :--> PInteger :--> PByteString :--> PByteString)
psliceBS = punsafeBuiltin PLC.SliceByteString

-- | Find the length of a 'PByteString'.
plengthBS :: Term s (PByteString :--> PInteger)
plengthBS = punsafeBuiltin PLC.LengthOfByteString

-- | 'PByteString' indexing function.
pindexBS :: Term s (PByteString :--> PInteger :--> PInteger)
pindexBS = punsafeBuiltin PLC.IndexByteString

hexDigitToWord8 :: HasCallStack => Char -> Word8
hexDigitToWord8 = f . toLower
  where
    f '0' = 0
    f '1' = 1
    f '2' = 2
    f '3' = 3
    f '4' = 4
    f '5' = 5
    f '6' = 6
    f '7' = 7
    f '8' = 8
    f '9' = 9
    f 'a' = 10
    f 'b' = 11
    f 'c' = 12
    f 'd' = 13
    f 'e' = 14
    f 'f' = 15
    f c = error $ "InvalidHexDigit " ++ [c]
