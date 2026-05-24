module Plutarch.Backend.RawTerm (
  VarTag (..),
  RawTerm (..),
) where

import Data.Vector (Vector)
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Word (Word64)
import Plutarch.Backend.PosTree (PosTree)
import Plutarch.Backend.UPLC (UPLCTerm)
import PlutusCore (Some, ValueOf)
import PlutusCore qualified as PLC

data VarTag
  = Argument
  | LetBinding
  | Self
  deriving stock (Show, Eq)

data RawTerm
  = RVar VarTag
  | RLamAbs (NonEmptyVector (Maybe PosTree)) RawTerm
  | RApply RawTerm (NonEmptyVector RawTerm)
  | RForce RawTerm
  | RDelay RawTerm
  | RConstant (Some (ValueOf PLC.DefaultUni))
  | RBuiltin PLC.DefaultFun
  | RCompiled UPLCTerm
  | RError
  | RPlaceholder Integer
  | RConstr Word64 (Vector RawTerm)
  | RCase RawTerm (NonEmptyVector RawTerm)
  | RLet (Maybe PosTree) RawTerm RawTerm
  | RFix PosTree RawTerm
  deriving stock (Eq, Show)
