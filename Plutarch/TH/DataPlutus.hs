{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.TH.DataPlutus (
  deriveDataPlutus,
) where

import Data.Foldable (for_, traverse_)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Language.Haskell.TH (
  Bang,
  BndrVis,
  Con (InfixC, NormalC, RecC),
  Dec,
  Name,
  Q,
  TyVarBndr,
  Type (AppT, ConT),
 )
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))
import Plutarch.Primitive.CanData (PCanData)
import Plutarch.Primitive.Data (PAsData, PData)
import Plutarch.Primitive.Eq (PEq (peq))
import Plutarch.TH.Helpers (fullTypeName, mkContextOf)

-- | @since wip
deriveDataPlutus :: Vector (TyVarBndr BndrVis) -> Name -> Vector Con -> Q [Dec]
deriveDataPlutus tvbs name constructors = case Vector.unsnoc constructors of
  Nothing -> fail "DataPlutus derivation is not possible for nullary types."
  Just (_, _) -> do
    traverse_ checkFieldIsWrapped constructors
    plutarchTypeDec <- derivePlutarchType tvbs name
    peqDec <- derivePEq tvbs name
    pure $ plutarchTypeDec <> peqDec

-- Helpers

derivePlutarchType :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePlutarchType tyVars tyName =
  [d|
    instance $ctx => PlutarchType $name where
      type PRepresentation $name = PData
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PCanData $ tyVars

derivePEq :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePEq tyVars tyName =
  [d|
    instance $ctx => PEq $name where
      peq = plam' $ \x -> plam' $ \y -> pequalsData # pcoerce x # pcoerce y
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PCanData $ tyVars

checkFieldIsWrapped :: Con -> Q ()
checkFieldIsWrapped = \case
  NormalC name fields -> for_ fields (go name)
  RecC name fields -> for_ fields (goNamed name)
  InfixC lhs name rhs -> do
    go name lhs
    go name rhs
  _ -> fail "Unexpected constructor type found. If you see this message, report as a bug."
  where
    go :: Name -> (Bang, Type) -> Q ()
    go conName (_, t) =
      let errMsg =
            "Constructor "
              <> show conName
              <> "has a field whose type is not wrapped in 'PAsData'."
       in dig errMsg t
    goNamed :: Name -> (Name, Bang, Type) -> Q ()
    goNamed conName (fieldName, _, t) =
      let errMsg =
            "Constructor "
              <> show conName
              <> "has a field whose type is not wrapped in 'PAsData', specifically "
              <> show fieldName
              <> "."
       in dig errMsg t
    dig :: String -> Type -> Q ()
    dig errMsg = \case
      AppT _ (AppT (ConT t) _) ->
        if t == ''PAsData
          then pure ()
          else fail errMsg
      _ -> fail errMsg
