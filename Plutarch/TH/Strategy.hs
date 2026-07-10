{-# LANGUAGE TemplateHaskellQuotes #-}

module Plutarch.TH.Strategy (
  Strategy (..),
  deriveFor,
) where

import Data.Foldable (foldl')
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Language.Haskell.TH (
  BndrVis,
  Con (ForallC, GadtC, InfixC, NormalC, RecC, RecGadtC),
  Dec (DataD, NewtypeD, TySynD),
  Info (TyConI),
  Name,
  Q,
  TyVarBndr (KindedTV, PlainTV),
  Type (AppT, ConT, TupleT, VarT),
  reify,
 )
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))
import Plutarch.Primitive.SOP (PSOP)

-- | @since wip
data Strategy = SOP

-- | @since wip
deriveFor :: Name -> Strategy -> Q [Dec]
deriveFor tyName strat = do
  tyDec <- checkTyName tyName
  case tyDec of
    DataD _ name tyVarBinds _ constructors _ -> case strat of
      SOP -> do
        let tvbAsVec = Vector.fromList tyVarBinds
        case Vector.unsnoc tvbAsVec of
          Nothing -> fail "Types must have an s :: S type parameter in last position."
          Just (tvbs, _) -> case constructors of
            [] -> fail "Nullary types do not support an SOP derivation strategy."
            (_ : _) ->
              if all hasNoFields constructors
                then fail "Use the Enum strategy for types with no fields in any 'arm'."
                else derivePlutarchType tvbs name
    NewtypeD {} -> case strat of
      SOP -> fail "Use the Newtype strategy for newtypes."
    TySynD {} -> fail "Type synonyms are not supported. Use the underlying type."
    _ -> fail "Not a valid type name."

-- Helpers

derivePlutarchType :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePlutarchType tyVars tyName =
  [d|
    instance $ctx => PlutarchType $name where
      type PRepresentation $name = PSOP
    |]
  where
    name :: Q Type
    name = pure $ foldl' (\acc -> AppT acc . VarT . bindToName) (ConT tyName) tyVars
    ctx :: Q Type
    ctx = do
      let len = Vector.length tyVars
      let varNames = fmap (AppT (ConT ''PlutarchType) . VarT . bindToName) tyVars
      pure $ foldl' AppT (TupleT len) varNames

checkTyName :: Name -> Q Dec
checkTyName tyName = do
  tyInfo <- reify tyName
  case tyInfo of
    TyConI tyDec -> pure tyDec
    _ -> fail $ show tyName <> " does not name a type."

hasNoFields :: Con -> Bool
hasNoFields = \case
  NormalC _ fields -> null fields
  RecC _ fields -> null fields
  InfixC {} -> False
  ForallC _ _ con -> hasNoFields con
  GadtC _ fields _ -> null fields
  RecGadtC _ fields _ -> null fields

bindToName :: TyVarBndr BndrVis -> Name
bindToName = \case
  PlainTV n _ -> n
  KindedTV n _ _ -> n
