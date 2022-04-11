{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.String (PString, pfromText, pencodeUtf8, pdecodeUtf8) where

import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as Txt
import Plutarch.Bool (PEq, (#==))
import Plutarch.ByteString (PByteString)
import Plutarch.Internal.Other (
  Term,
  (#),
  type (:-->),
 )
import Plutarch.Lift (
  DerivePConstantDirect (DerivePConstantDirect),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
  pconstant,
 )
import Plutarch.Unsafe (punsafeBuiltin)
import qualified PlutusCore as PLC

-- | Plutus 'BuiltinString' values
data PString s

instance PUnsafeLiftDecl PString where type PLifted PString = Text
deriving via (DerivePConstantDirect Text PString) instance PConstantDecl Text

{-# DEPRECATED pfromText "Use `pconstant` instead." #-}

-- | Create a PString from 'Text'
pfromText :: Txt.Text -> Term s PString
pfromText = pconstant

instance IsString (Term s PString) where
  fromString = pconstant . Txt.pack

instance PEq PString where
  x #== y = punsafeBuiltin PLC.EqualsString # x # y

instance Semigroup (Term s PString) where
  x <> y = punsafeBuiltin PLC.AppendString # x # y

instance Monoid (Term s PString) where
  mempty = pconstant Txt.empty

-- | Encode a 'PString' using UTF-8.
pencodeUtf8 :: Term s (PString :--> PByteString)
pencodeUtf8 = punsafeBuiltin PLC.EncodeUtf8

-- | Decode a 'PByteString' using UTF-8.
pdecodeUtf8 :: Term s (PByteString :--> PString)
pdecodeUtf8 = punsafeBuiltin PLC.DecodeUtf8
