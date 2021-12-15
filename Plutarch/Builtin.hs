module Plutarch.Builtin (PData (..), pfstBuiltin, psndBuiltin, pasConstr, preadByteStr, PBuiltinPair, PBuiltinList, PBuiltinByteString, PBuiltinString) where

import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.String (IsString (..))
import Data.Word (Word8)
import Plutarch (punsafeBuiltin, punsafeConstant)
import Plutarch.Bool (PEq (..), POrd (..))
import Plutarch.Integer (PInteger)
import Plutarch.Prelude
import qualified PlutusCore as PLC

data PBuiltinPair (a :: k -> Type) (b :: k -> Type) (s :: k)

data PBuiltinList (a :: k -> Type) (s :: k)

data PBuiltinByteString s

instance PEq PBuiltinByteString where
  x £== y = punsafeBuiltin PLC.EqualsByteString £ x £ y

instance POrd PBuiltinByteString where
  x £<= y = punsafeBuiltin PLC.LessThanEqualsByteString £ x £ y
  x £< y = punsafeBuiltin PLC.LessThanByteString £ x £ y

-- | Errors that may arise while using 'preadByteStr'.
data ByteStringReadError = UnevenLength | InvalidHexDigit Char
  deriving stock (Eq, Ord, Read, Show)

-- | Interpret a hex string as a PBuiltinByteString.
preadByteStr :: String -> Either ByteStringReadError (Term s PBuiltinByteString)
preadByteStr = fmap (punsafeConstant . PLC.Some . PLC.ValueOf PLC.DefaultUniByteString . BS.pack) . f
  where
    f "" = Right []
    f [_] = Left UnevenLength
    f (x : y : rest) =
      (\a b s -> (a * 16 + b) : s)
        <$> hexDigitToWord8 x
        <*> hexDigitToWord8 y
        <*> f rest

data PBuiltinString s

instance IsString (Term s PBuiltinString) where
  fromString = punsafeConstant . PLC.Some . PLC.ValueOf PLC.DefaultUniString . fromString

instance PEq PBuiltinString where
  x £== y = punsafeBuiltin PLC.EqualsString £ x £ y

data PData s
  = PDataConstr (Term s (PBuiltinPair PInteger (PBuiltinList PData)))
  | PDataMap (Term s (PBuiltinList (PBuiltinPair PData PData)))
  | PDataList (Term s (PBuiltinList PData))
  | PDataInteger (Term s PInteger)
  | PDataByteString (Term s PBuiltinByteString)

pfstBuiltin :: Term s (PBuiltinPair a b :--> a)
pfstBuiltin = phoistAcyclic $ pforce . pforce . punsafeBuiltin $ PLC.FstPair

psndBuiltin :: Term s (PBuiltinPair a b :--> b)
psndBuiltin = phoistAcyclic $ pforce . pforce . punsafeBuiltin $ PLC.SndPair

pasConstr :: Term s (PData :--> PBuiltinPair PInteger (PBuiltinList PData))
pasConstr = punsafeBuiltin PLC.UnConstrData

hexDigitToWord8 :: Char -> Either ByteStringReadError Word8
hexDigitToWord8 = f . toLower
  where
    f '0' = Right 0
    f '2' = Right 2
    f '3' = Right 3
    f '4' = Right 4
    f '5' = Right 5
    f '6' = Right 6
    f '7' = Right 7
    f '8' = Right 8
    f '9' = Right 9
    f 'a' = Right 10
    f 'b' = Right 11
    f 'c' = Right 12
    f 'd' = Right 13
    f 'e' = Right 14
    f 'f' = Right 15
    f c = Left $ InvalidHexDigit c
