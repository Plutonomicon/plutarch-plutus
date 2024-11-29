{-# LANGUAGE FlexibleInstances #-}

module Plutarch.Builtin.String where

import Data.Kind (Type)
import Data.String (IsString, fromString)
import Data.Text qualified as Text
import GHC.Generics (Generic)

import Plutarch.Builtin.ByteString
import Plutarch.Builtin.Integer
import Plutarch.Builtin.Opaque

import Plutarch.Internal.Term
import PlutusCore qualified as PLC

-- | Plutus 'BuiltinString' values
newtype PString s = PString (Term s POpaque)
  deriving stock (Generic)

instance IsString (Term s PString) where
  fromString = punsafeConstantInternal . PLC.someValue . Text.pack

instance Semigroup (Term s PString) where
  x <> y = punsafeBuiltin PLC.AppendString # x # y

instance Monoid (Term s PString) where
  mempty = punsafeConstantInternal $ PLC.someValue Text.empty

-- | Encode a 'PString' using UTF-8.
pencodeUtf8 :: Term s (PString :--> PByteString)
pencodeUtf8 = punsafeBuiltin PLC.EncodeUtf8

-- | Decode a 'PByteString' using UTF-8.
pdecodeUtf8 :: Term s (PByteString :--> PString)
pdecodeUtf8 = punsafeBuiltin PLC.DecodeUtf8

ptrace' :: Term s (PString :--> a :--> a)
ptrace' = phoistAcyclic $ pforce $ punsafeBuiltin PLC.Trace

{- | Trace the given message at the info level before evaluating the given
argument.

@since 1.6.0
-}
ptraceInfo ::
  forall (a :: S -> Type) (s :: S).
  Term s PString ->
  Term s a ->
  Term s a
ptraceInfo msg x = pgetConfig $ \case
  NoTracing -> x
  Tracing _ _ -> pforce $ ptrace' # msg # pdelay x
