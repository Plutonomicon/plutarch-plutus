{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.String (
  -- * Type
  PString,
  -- Functions
  pisHexDigit,
  pfromText,
  pencodeUtf8,
  pdecodeUtf8,
) where

import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Plutarch.Bool (PBool, PEq, (#&&), (#<=), (#==), (#||))
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Internal (S, Term, phoistAcyclic, (#), (:-->))
import Plutarch.Internal.Lift (DeriveBuiltinPLiftable, PLiftable, PLifted' (PLifted'))
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.Other (POpaque)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (DPTStrat, DerivePlutusType, PlutusType)
import Plutarch.Lift (
  DerivePConstantDirect (DerivePConstantDirect),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
  pconstant,
 )
import Plutarch.Unsafe (punsafeBuiltin)
import PlutusCore qualified as PLC

-- | Plutus 'BuiltinString' values
newtype PString s = PString (Term s POpaque)
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PString where type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  (DeriveBuiltinPLiftable PString Text)
  instance
    PLiftable PString

instance PUnsafeLiftDecl PString where type PLifted PString = Text

deriving via (DerivePConstantDirect Text PString) instance PConstantDecl Text

{-# DEPRECATED pfromText "Use `pconstant` instead." #-}

-- | Create a PString from 'Text'
pfromText :: Text.Text -> Term s PString
pfromText = pconstant

instance IsString (Term s PString) where
  fromString = pconstant . Text.pack

instance PEq PString where
  x #== y = punsafeBuiltin PLC.EqualsString # x # y

instance Semigroup (Term s PString) where
  x <> y = punsafeBuiltin PLC.AppendString # x # y

instance Monoid (Term s PString) where
  mempty = pconstant Text.empty

-- | Encode a 'PString' using UTF-8.
pencodeUtf8 :: Term s (PString :--> PByteString)
pencodeUtf8 = punsafeBuiltin PLC.EncodeUtf8

-- | Decode a 'PByteString' using UTF-8.
pdecodeUtf8 :: Term s (PByteString :--> PString)
pdecodeUtf8 = punsafeBuiltin PLC.DecodeUtf8

{- | Verify if the given argument is the ASCII encoding of a hex digit. This
includes specifically the following ASCII ranges (inclusively):

* 48-54 (digits 0 through 9)
* 65-70 (upper-case A through upper-case F)
* 97-102 (lower-case a through lower-case f)

@since WIP
-}
pisHexDigit :: forall (s :: S). Term s (PInteger :--> PBool)
pisHexDigit = phoistAcyclic $ plam $ \c ->
  (c #<= 57 #&& 48 #<= c)
    #|| (c #<= 70 #&& 65 #<= c)
    #|| (c #<= 102 #&& 97 #<= c)
