{-# LANGUAGE FlexibleInstances #-}

module Plutarch.Builtin.String (
  -- * Type
  PString (..),

  -- * Builtins
  pbuiltinEqualsString,
  pbuiltinAppendString,
  pbuiltinEncodeUtf8,
  pbuiltinDecodeUtf8,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Exts (IsString (fromString))
import GHC.Generics (Generic)
import Plutarch.Builtin.Bool (PBool)
import Plutarch.Builtin.ByteString (PByteString)
import Plutarch.Builtin.Opaque (POpaque)
import Plutarch.Internal.Term (
  S,
  Term,
  punsafeBuiltin,
  punsafeConstantInternal,
  (#),
  (:-->),
 )
import PlutusCore qualified as PLC

{- | A Plutus string.

@since WIP
-}
newtype PString (s :: S) = PString (Term s POpaque)
  deriving stock
    ( -- | @since WIP
      Generic
    )

-- | @since WIP
instance Semigroup (Term s PString) where
  {-# INLINEABLE (<>) #-}
  x <> y = pbuiltinAppendString # x # y

-- | @since WIP
instance Monoid (Term s PString) where
  -- This is morally `pconstant ""`, but we can't depend on the module defining
  -- this function, as it _also_ contains the instance(s) required for PByteString
  -- to work, which would create a cyclic dependency.
  --
  -- This is a temporary hack that _should_ be obviated by PLiftable, but I'm
  -- putting this in here until this gets resolved.
  --
  -- Koz
  {-# INLINEABLE mempty #-}
  mempty = punsafeConstantInternal $ PLC.someValue @Text @PLC.DefaultUni ""

-- | @since WIP
instance IsString (Term s PString) where
  -- This is morally `pconstant`, but for similar reasons to the `Monoid`
  -- instance, we are forced to do it this way
  {-# INLINEABLE fromString #-}
  fromString = punsafeConstantInternal . PLC.someValue @Text @PLC.DefaultUni . Text.pack

-- | @since WIP
pbuiltinEqualsString ::
  forall (s :: S).
  Term s (PString :--> PString :--> PBool)
pbuiltinEqualsString = punsafeBuiltin PLC.EqualsString

-- | @since WIP
pbuiltinAppendString ::
  forall (s :: S).
  Term s (PString :--> PString :--> PString)
pbuiltinAppendString = punsafeBuiltin PLC.AppendString

-- | @since WIP
pbuiltinEncodeUtf8 ::
  forall (s :: S).
  Term s (PString :--> PByteString)
pbuiltinEncodeUtf8 = punsafeBuiltin PLC.EncodeUtf8

-- | @since WIP
pbuiltinDecodeUtf8 ::
  forall (s :: S).
  Term s (PByteString :--> PString)
pbuiltinDecodeUtf8 = punsafeBuiltin PLC.DecodeUtf8
