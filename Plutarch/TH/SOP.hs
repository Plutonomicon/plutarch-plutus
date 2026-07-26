{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.TH.SOP (
  deriveSOP,
) where

import Data.Foldable (foldl', foldlM, foldrM)
import Data.Traversable.WithIndex (itraverse)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty qualified as NEVector
import Language.Haskell.TH (
  BndrVis,
  Body (NormalB),
  Con,
  Dec,
  Exp (AppE, CaseE, ConE, LamE, LitE, VarE),
  Lit (IntegerL),
  Match (Match),
  Name,
  Pat (ConP, VarP, WildP),
  Q,
  TyVarBndr,
  Type,
  newName,
 )
import Plutarch.Backend.Term (plam')
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))
import Plutarch.Primitive.Con (PCon (pcon'))
import Plutarch.Primitive.Eq (PEq (peq))
import Plutarch.Primitive.Match (PMatch (pmatch'))
import Plutarch.Primitive.SOP (PSOP)
import Plutarch.TH.Helpers (
  conToName,
  fullTypeName,
  getArity,
  hasNoFields,
  mkContextOf,
  plam'E,
  punsafeCaseE,
  punsafeConstrE,
  toSomeTermE,
 )

-- | @since wip
deriveSOP :: Vector (TyVarBndr BndrVis) -> Name -> Vector Con -> Q [Dec]
deriveSOP tvbs name constructors = case Vector.unsnoc constructors of
  Nothing -> fail "SOP derivation is not possible for nullary types."
  Just (cs, c) ->
    if Vector.all hasNoFields constructors
      then fail "Use the Enum strategy for types with no fields in any 'arm'."
      else do
        plutarchTypeDec <- derivePlutarchType tvbs name
        pmatchDec <- derivePMatch tvbs name cs c
        pconDec <- derivePCon tvbs name constructors
        peqDec <- derivePEq tvbs name constructors
        pure $ plutarchTypeDec <> pmatchDec <> pconDec <> peqDec

-- Helpers

derivePlutarchType :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePlutarchType tyVars tyName =
  [d|
    instance $ctx => PlutarchType $name where
      type PRepresentation $name = PSOP
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
      pmatch' x f = let handlers = $(mkHandlers 'f) in $punsafeCaseE x handlers
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PlutarchType $ tyVars
    -- We need one handler for each 'arm', of appropriate arity for the field
    -- counts.
    --
    -- We know there's at least one arm because we checked before we made it
    -- here.
    mkHandlers :: Name -> Q Exp
    mkHandlers contName = do
      let arityLast = getArity c
      nameLast <- conToName c
      let aritiesRest = fmap getArity cs
      namesRest <- traverse conToName cs
      handlerLast <- mkHandler contName (nameLast, arityLast)
      handlersRest <- traverse (mkHandler contName) (Vector.zip namesRest aritiesRest)
      start <- [e|NEVector.singleton ($toSomeTermE $(pure handlerLast))|]
      foldrM (\e acc -> [e|NEVector.cons ($toSomeTermE $(pure e)) $(pure acc)|]) start handlersRest
    mkHandler :: Name -> (Name, Word) -> Q Exp
    mkHandler contName (conName, arity) = do
      argNames <- case arity of
        0 -> pure []
        n -> traverse (\i -> newName $ "x" <> show i) [0, 1 .. n - 1]
      let conCallExp = AppE (VarE contName) . foldl' (\acc -> AppE acc . VarE) (ConE conName) $ argNames
      pure . foldr (\name -> AppE (VarE 'plam') . LamE [VarP name]) conCallExp $ argNames

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
      let arity = getArity con
      conName <- conToName con
      fieldNames <- case arity of
        0 -> pure []
        n -> traverse (\i -> newName $ "f" <> show i) [0, 1 .. n - 1]
      let conMatchPat = ConP conName [] . fmap VarP $ fieldNames
      let constrIx = LitE . IntegerL . fromIntegral $ conIx
      constrVec <- foldrM (\n acc -> [e|Vector.cons ($toSomeTermE $(pure (VarE n))) $(pure acc)|]) (VarE 'Vector.empty) fieldNames
      matchBody <- [e|$punsafeConstrE $(pure constrIx) $(pure constrVec)|]
      pure . Match conMatchPat (NormalB matchBody) $ []

derivePEq :: Vector (TyVarBndr BndrVis) -> Name -> Vector Con -> Q [Dec]
derivePEq tyVars tyName constructors =
  [d|
    instance $ctx => PEq $name where
      peq = $plam'E $ \x -> $plam'E $ \y -> pmatch x $ \xInner ->
        pmatch y $ \yInner ->
          $(peqImpl 'xInner 'yInner)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PEq $ tyVars
    peqImpl :: Name -> Name -> Q Exp
    peqImpl xName yName = do
      matches <- Vector.toList <$> traverse (mkMatch yName) constructors
      pure . CaseE (VarE xName) $ matches
    mkMatch :: Name -> Con -> Q Match
    mkMatch yName con = do
      let arity = getArity con
      conName <- conToName con
      fieldNamesX <- case arity of
        0 -> pure []
        n -> traverse (\i -> newName $ "x" <> show i) [0, 1 .. n - 1]
      fieldNamesY <- case arity of
        0 -> pure []
        n -> traverse (\i -> newName $ "y" <> show i) [0, 1 .. n - 1]
      let xMatchPat = ConP conName [] . fmap VarP $ fieldNamesX
      let yMatchPat = ConP conName [] . fmap VarP $ fieldNamesY
      hitExp <- case zip fieldNamesX fieldNamesY of
        [] -> [e|ptrue|]
        (xField, yField) : fields -> do
          let xVar = VarE xField
          let yVar = VarE yField
          start <- [e|peq # $(pure xVar) # $(pure yVar)|]
          foldlM mkPand start fields
      missExp <- [e|pfalse|]
      let matchBody = CaseE (VarE yName) [Match yMatchPat (NormalB hitExp) [], Match WildP (NormalB missExp) []]
      pure . Match xMatchPat (NormalB matchBody) $ []
    mkPand :: Exp -> (Name, Name) -> Q Exp
    mkPand acc (xName, yName) = do
      let xVar = VarE xName
      let yVar = VarE yName
      [e|pand (peq # $(pure xVar) # $(pure yVar)) $(pure acc)|]
