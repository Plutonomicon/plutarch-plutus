{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Plutarch.FFI (
  Delayed,
  foreignExport,
  foreignImport,
  unsafeForeignExport,
  unsafeForeignImport,
) where

import Data.ByteString (ByteString)
import Data.Kind (Constraint, Type)
import Data.Text (Text)
import GHC.Generics (C, D, K1, M1, Meta (MetaData), Rep, S)
import GHC.TypeLits (TypeError)
import qualified GHC.TypeLits as TypeLits
import qualified Generics.SOP as SOP
import Plutarch.Bool (PBool)
import Plutarch.Builtin (PAsData, PData)
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Internal (
  ClosedTerm,
  PDelayed,
  PType,
  RawTerm (RCompiled),
  Term (Term),
  TermResult (TermResult),
  asClosedRawTerm,
  compile',
  (:-->),
 )
import Plutarch.Internal.PlutusType (PlutusType (PInner))
import Plutarch.String (PString)
import Plutus.V1.Ledger.Api (
  PubKeyHash (..),
 )
import Plutus.V1.Ledger.Scripts (Script (unScript), fromCompiledCode)
import PlutusTx.Builtins.Internal (BuiltinBool, BuiltinByteString, BuiltinData)
import PlutusTx.Code (CompiledCode, CompiledCodeIn (DeserializedCode))
import PlutusTx.Prelude (BuiltinString)
import UntypedPlutusCore (fakeNameDeBruijn)
import qualified UntypedPlutusCore as UPLC

data ForallPhantom :: Type
data PhorallPhantom :: PType

data Delayed :: Type -> Type

foreignExport :: PlutarchInner p PhorallPhantom ~~ PlutusTxInner t ForallPhantom => ClosedTerm p -> CompiledCode t
foreignExport = unsafeForeignExport

foreignImport :: PlutarchInner p PhorallPhantom ~~ PlutusTxInner t ForallPhantom => CompiledCode t -> ClosedTerm p
foreignImport = unsafeForeignImport

unsafeForeignExport :: ClosedTerm p -> CompiledCode t
unsafeForeignExport t = DeserializedCode program Nothing mempty
  where
    program =
      UPLC.Program () (UPLC.Version () 1 0 0) $
        UPLC.termMapNames fakeNameDeBruijn $
          compile' $
            asClosedRawTerm t

unsafeForeignImport :: CompiledCode t -> ClosedTerm p
unsafeForeignImport c = Term $ const $ TermResult (RCompiled $ UPLC.toTerm $ unScript $ fromCompiledCode c) []

type family a ~~ b :: Constraint where
  ForallPhantom ~~ _ = ()
  _ ~~ ForallPhantom = ()
  Delayed a ~~ Delayed b = a ~~ b
  a ~~ b = a ~ b

type family PlutarchInner (p :: PType) (any :: PType) :: Type where
  PlutarchInner PBool _ = BuiltinBool
  PlutarchInner PInteger _ = Integer
  PlutarchInner PString _ = Text
  PlutarchInner PByteString _ = ByteString
  PlutarchInner PData _ = BuiltinData
  PlutarchInner PhorallPhantom _ = ForallPhantom
  PlutarchInner (PAsData a :--> PAsData b) x = PlutarchInner (PData :--> PData) x
  PlutarchInner (PAsData a :--> b) x = PlutarchInner (PData :--> b) x
  PlutarchInner (a :--> b) x = PlutarchInner a x -> PlutarchInner b x
  PlutarchInner (PDelayed a) x = Delayed (PlutarchInner a x)
  PlutarchInner p x = PlutarchInner (PInner p x) x

type family PlutusTxInner (t :: Type) (any :: Type) :: Type where
  PlutusTxInner BuiltinBool _ = BuiltinBool
  PlutusTxInner Integer _ = Integer
  PlutusTxInner BuiltinString _ = Text
  PlutusTxInner BuiltinByteString _ = ByteString
  PlutusTxInner BuiltinData _ = BuiltinData
  PlutusTxInner ForallPhantom _ = ForallPhantom
  PlutusTxInner (a -> b) x = PlutusTxInner a x -> PlutusTxInner b x
  PlutusTxInner (Delayed a) x = Delayed (PlutusTxInner a x)
  PlutusTxInner a x = TypeEncoding a (Rep a) x

type TypeEncoding :: Type -> (Type -> Type) -> Type -> Type
type family TypeEncoding a rep x where
  TypeEncoding a (M1 D ( 'MetaData _ _ _ 'True) (M1 C _ (M1 S _ (K1 _ b)))) x = PlutusTxInner b x -- newtype
  TypeEncoding a _ x = Delayed (PlutusTxInner (ScottFn (ScottList (SOP.Code a) x) x) x)

{- |
  List of scott-encoded constructors of a Haskell type (represented by 'SOP.Code')

  ScottList (Code (Either a b)) c = '[a -> c, b -> c]
-}
type ScottList :: [[Type]] -> Type -> [Type]
type family ScottList code c where
-- We disallow certain shapes because Scott encoding is not appropriate for them.
  ScottList '[] c = TypeError ( 'TypeLits.Text "PlutusType(scott encoding): Data type without constructors not accepted")
  ScottList '[ '[]] c =
    TypeError
      ( 'TypeLits.Text
          "PlutusType(scott encoding): Data type with single nullary constructor not accepted"
      )
  ScottList '[ '[_]] c =
    TypeError
      ( 'TypeLits.Text
          "PlutusType(scott encoding): Data type with single unary constructor not accepted; use newtype!"
      )
  ScottList (xs ': xss) c = ScottFn xs c ': ScottList' xss c

type ScottList' :: [[Type]] -> Type -> [Type]
type family ScottList' code c where
  ScottList' '[] c = '[]
  ScottList' (xs ': xss) c = ScottFn xs c ': ScottList' xss c

{- |
  An individual constructor function of a Scott encoding.

   ScottFn '[a, b] c = (a -> b -> c)
   ScottFn '[] c = c
-}
type ScottFn :: [Type] -> Type -> Type
type family ScottFn xs b where
  ScottFn '[] b = b
  ScottFn (x ': xs) b = x -> ScottFn xs b
