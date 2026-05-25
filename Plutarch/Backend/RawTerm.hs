module Plutarch.Backend.RawTerm (
  VarTag (..),
  RawTerm (..),
  getRawTermAnn,
) where

import Data.Kind (Type)
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

data RawTerm (ann :: Type)
  = RVar ann VarTag
  | RLamAbs ann (NonEmptyVector (Maybe PosTree)) (RawTerm ann)
  | RApply ann (RawTerm ann) (NonEmptyVector (RawTerm ann))
  | RForce ann (RawTerm ann)
  | RDelay ann (RawTerm ann)
  | RConstant ann (Some (ValueOf PLC.DefaultUni))
  | RBuiltin ann PLC.DefaultFun
  | RCompiled ann UPLCTerm
  | RError ann
  | RPlaceholder ann Integer
  | RConstr ann Word64 (Vector (RawTerm ann))
  | RCase ann (RawTerm ann) (NonEmptyVector (RawTerm ann))
  | RLet ann (Maybe PosTree) (RawTerm ann) (RawTerm ann)
  | RFix ann PosTree (RawTerm ann)
  deriving stock (Eq, Show)

getRawTermAnn ::
  forall (ann :: Type).
  RawTerm ann -> ann
getRawTermAnn = \case
  RVar x _ -> x
  RLamAbs x _ _ -> x
  RApply x _ _ -> x
  RForce x _ -> x
  RDelay x _ -> x
  RConstant x _ -> x
  RBuiltin x _ -> x
  RCompiled x _ -> x
  RError x -> x
  RPlaceholder x _ -> x
  RConstr x _ _ -> x
  RCase x _ _ -> x
  RLet x _ _ _ -> x
  RFix x _ _ -> x
