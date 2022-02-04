{-# LANGUAGE UndecidableInstances #-}

module Plutarch.FFI (
  foreignExport,
  foreignImport,
) where

import Data.Kind (Type)
import Plutarch.Internal (ClosedTerm, PType, RawTerm (RCompiled), Term (Term), TermResult (TermResult), asClosedRawTerm, compile', (:-->))
import Plutarch.Lift (PLifted)
import Plutus.V1.Ledger.Scripts (Script (unScript), fromCompiledCode)
import PlutusTx.Code (CompiledCode, CompiledCodeIn (DeserializedCode))
import UntypedPlutusCore (fakeNameDeBruijn)
import qualified UntypedPlutusCore as UPLC

data ForallPhantom

foreignExport :: PlutarchInner p ForallPhantom ~ PlutusTxInner t ForallPhantom => ClosedTerm p -> CompiledCode t
foreignExport t = DeserializedCode program Nothing mempty
  where
    program =
      UPLC.Program () (UPLC.Version () 1 0 0) $
        UPLC.termMapNames fakeNameDeBruijn $
          compile' $
            asClosedRawTerm t

foreignImport :: PlutarchInner p ForallPhantom ~ PlutusTxInner t ForallPhantom => CompiledCode t -> ClosedTerm p
foreignImport c = Term $ const $ TermResult (RCompiled $ UPLC.toTerm $ unScript $ fromCompiledCode c) []

type family PlutarchInner (p :: PType) (any :: Type) :: Type where
  PlutarchInner (a :--> b) x = PlutarchInner a x -> PlutarchInner b x
  PlutarchInner p _ = PLifted p

type family PlutusTxInner (t :: Type) (any :: Type) :: Type where
  PlutusTxInner Bool _ = Bool
  PlutusTxInner Integer _ = Integer
  PlutusTxInner (a -> b) x = PlutusTxInner a x -> PlutusTxInner b x
