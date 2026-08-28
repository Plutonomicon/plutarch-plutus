{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.TH.Enum (
  deriveEnum,
) where

import Data.Foldable (foldrM)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Language.Haskell.TH (
  BndrVis,
  Body (NormalB),
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
  PTypeProduct (PTypeProduct),
  PTypeSum (PTypeSum),
  fullTypeName,
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
deriveEnum :: Vector (TyVarBndr BndrVis) -> Name -> PTypeSum -> Q [Dec]
deriveEnum tvbs name (PTypeSum typeStructure) = do
  skeleton <- traverse asEmptyProduct typeStructure
  plutarchTypeDec <- derivePlutarchType tvbs name
  pmatchDec <- derivePMatch tvbs name skeleton
  pconDec <- derivePCon tvbs name skeleton
  peqDec <- derivePEq tvbs name
  pure $ plutarchTypeDec <> pmatchDec <> pconDec <> peqDec

-- Helpers

asEmptyProduct :: (Name, PTypeProduct) -> Q Name
asEmptyProduct (name, PTypeProduct fields) = case Vector.uncons fields of
  Nothing -> pure name
  Just (_, _) -> fail "Cannot derive using Enum strategy for types with fields in any 'arm'."

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

derivePMatch :: Vector (TyVarBndr BndrVis) -> Name -> NonEmptyVector Name -> Q [Dec]
derivePMatch tyVars tyName constructorNames =
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
      let (namesRest, nameLast) = NEVector.unsnoc constructorNames
      handlerLast <- mkHandler contName nameLast
      handlersRest <- traverse (mkHandler contName) namesRest
      start <- [e|NEVector.singleton ($toSomeTermE $(pure handlerLast))|]
      foldrM (\e acc -> [e|NEVector.cons ($toSomeTermE $(pure e)) $(pure acc)|]) start handlersRest
    mkHandler :: Name -> Name -> Q Exp
    mkHandler contName conName = [e|$(pure (VarE contName)) $(pure (ConE conName))|]

derivePCon :: Vector (TyVarBndr BndrVis) -> Name -> NonEmptyVector Name -> Q [Dec]
derivePCon tyVars tyName constructorNames =
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
    matches bindName = do
      matchVec <- NEVector.imapM mkMatch constructorNames
      pure . CaseE (VarE bindName) . NEVector.toList $ matchVec
    mkMatch :: Int -> Name -> Q Match
    mkMatch conIx conName = do
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
