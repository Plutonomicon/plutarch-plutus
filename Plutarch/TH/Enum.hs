{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.TH.Enum (
  deriveEnum,
) where

import Data.Foldable (foldrM)
import Data.Traversable.WithIndex (itraverse)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty qualified as NEVector
import Language.Haskell.TH (
  BndrVis,
  Body (NormalB),
  Con,
  Dec,
  Exp (CaseE, ConE, LitE, VarE),
  Lit (IntegerL),
  Match (Match),
  Name,
  Pat (ConP),
  Q,
  TyVarBndr,
  Type,
 )
import Plutarch.Helpers.TH (
  conToName,
  fullTypeName,
  hasNoFields,
  mkContextOf,
  pequalsIntegerE,
  plam'E,
  punsafeCaseE,
  punsafeConstantE,
  toSomeTermE,
 )
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))
import Plutarch.Primitive.Con (PCon (pcon'))
import Plutarch.Primitive.Eq (PEq (peq))
import Plutarch.Primitive.Match (PMatch (pmatch'))
import Plutarch.Primitive.Numeric (PInteger)
import PlutusCore qualified as PLC

-- | @since wip
deriveEnum :: Vector (TyVarBndr BndrVis) -> Name -> Vector Con -> Q [Dec]
deriveEnum tvbs name constructors = case Vector.unsnoc constructors of
  Nothing -> fail "Enum derivation is not possible for nullary types."
  Just (cs, c) ->
    if Vector.all hasNoFields constructors
      then do
        plutarchTypeDec <- derivePlutarchType tvbs name
        pmatchDec <- derivePMatch tvbs name cs c
        pconDec <- derivePCon tvbs name constructors
        peqDec <- derivePEq tvbs name
        pure $ plutarchTypeDec <> pmatchDec <> pconDec <> peqDec
      else fail "Cannot derive using Enum strategy for types with fields in any 'arm'."

-- Helpers

derivePlutarchType :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePlutarchType tyVars tyName =
  [d|
    instance $ctx => PlutarchType $name where
      type PRepresentation $name = PInteger
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PlutarchType $ tyVars

derivePMatch :: Vector (TyVarBndr BndrVis) -> Name -> Vector Con -> Con -> Q [Dec]
derivePMatch tyVars tyName cs c =
  [d|
    instance $ctx => PMatch $name where
      pmatch' x f = $punsafeCaseE x $(mkHandlers 'f)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PlutarchType $ tyVars
    mkHandlers :: Name -> Q Exp
    mkHandlers contName = do
      nameLast <- conToName c
      namesRest <- traverse conToName cs
      handlerLast <- mkHandler contName nameLast
      handlersRest <- traverse (mkHandler contName) namesRest
      start <- [e|NEVector.singleton ($toSomeTermE $(pure handlerLast))|]
      foldrM (\e acc -> [e|NEVector.cons ($toSomeTermE $(pure e)) $(pure acc)|]) start handlersRest
    mkHandler :: Name -> Name -> Q Exp
    mkHandler contName conName = [e|$(pure (VarE contName)) $(pure (ConE conName))|]

derivePCon :: Vector (TyVarBndr BndrVis) -> Name -> Vector Con -> Q [Dec]
derivePCon tyVars tyName constructors =
  [d|
    instance $ctx => PCon $name where
      pcon' x = $(matches 'x)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PlutarchType $ tyVars
    matches :: Name -> Q Exp
    matches bindName = CaseE (VarE bindName) . Vector.toList <$> itraverse mkMatch constructors
    mkMatch :: Int -> Con -> Q Match
    mkMatch conIx con = do
      conName <- conToName con
      let conMatchPat = ConP conName [] []
      let constrIx = LitE . IntegerL . fromIntegral $ conIx
      constrE <- [e|$punsafeConstantE (PLC.someValue @Integer $(pure constrIx))|]
      pure . Match conMatchPat (NormalB constrE) $ []

derivePEq :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePEq tyVars tyName =
  [d|
    instance $ctx => PEq $name where
      peq = $plam'E $ \x -> $plam'E $ \y -> $pequalsIntegerE # pcoerce x # pcoerce y
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PlutarchType $ tyVars
