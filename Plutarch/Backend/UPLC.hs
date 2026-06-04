{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.Backend.UPLC (
  UPLCTerm (..),
  uplcApply,
  uplcLam,
  uplcLet,
  uplcForce,
  uplcDelay,
  uplcVar,
  uplcConstant,
  uplcBuiltin,
  uplcError,
  uplcConstr,
  uplcCase,
) where

import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Data.Word (Word64)
import PlutusCore (Some, ValueOf)
import UntypedPlutusCore qualified as UPLC

newtype UPLCTerm = UPLCTerm (UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ())
  deriving (Eq, Show, Hashable) via (UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ())

uplcLet :: UPLC.Name -> UPLCTerm -> UPLCTerm -> UPLCTerm
uplcLet varName v f = uplcApply (uplcLam1 varName f) v

uplcApply :: UPLCTerm -> UPLCTerm -> UPLCTerm
uplcApply (UPLCTerm f) (UPLCTerm x) = UPLCTerm . UPLC.Apply () f $ x

uplcForce :: UPLCTerm -> UPLCTerm
uplcForce (UPLCTerm x) = UPLCTerm . UPLC.Force () $ x

uplcDelay :: UPLCTerm -> UPLCTerm
uplcDelay (UPLCTerm x) = UPLCTerm . UPLC.Delay () $ x

uplcVar :: UPLC.Name -> UPLCTerm
uplcVar = UPLCTerm . UPLC.Var ()

uplcConstant :: Some (ValueOf UPLC.DefaultUni) -> UPLCTerm
uplcConstant = UPLCTerm . UPLC.Constant ()

uplcBuiltin :: UPLC.DefaultFun -> UPLCTerm
uplcBuiltin = UPLCTerm . UPLC.Builtin ()

uplcError :: UPLCTerm
uplcError = UPLCTerm . UPLC.Error $ ()

uplcConstr :: Word64 -> Vector UPLCTerm -> UPLCTerm
uplcConstr tag = UPLCTerm . UPLC.Constr () tag . Vector.toList . fmap coerce

uplcCase :: UPLCTerm -> NonEmptyVector UPLCTerm -> UPLCTerm
uplcCase (UPLCTerm scrut) = UPLCTerm . UPLC.Case () scrut . NEVector.toVector . fmap coerce

uplcLam :: NonEmptyVector UPLC.Name -> UPLCTerm -> UPLCTerm
uplcLam argNames body = NEVector.foldr uplcLam1 body argNames

-- Helpers

uplcLam1 :: UPLC.Name -> UPLCTerm -> UPLCTerm
uplcLam1 varName (UPLCTerm body) = UPLCTerm . UPLC.LamAbs () varName $ body
