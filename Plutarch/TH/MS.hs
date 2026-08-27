{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.TH.MS (deriveMS) where

import Control.Monad (when)
import Data.Coerce (coerce)
import Data.Foldable (foldl', foldlM, foldrM)
import Data.Kind qualified as GHC
import Data.Traversable.WithIndex (itraverse)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Language.Haskell.TH (
  BndrVis,
  Body (NormalB),
  Dec (ValD),
  Exp (
    AppE,
    AppTypeE,
    CaseE,
    ConE,
    LamE,
    LetE,
    VarE
  ),
  Match (Match),
  Name,
  Pat (ConP, VarP, WildP),
  Q,
  TyVarBndr,
  Type (AppT, ConT, VarT, WildCardT),
  newName,
 )
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, papp, plam', punsafeCoerce)
import Plutarch.Helpers.TH (
  PType,
  PTypeProduct (PTypeProduct),
  PTypeSum (PTypeSum),
  bindToName,
  checkAndMark,
  fullTypeName,
  mkAnonPLam,
  mkContextOf,
  mkPLam,
  plam'E,
 )
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))
import Plutarch.Primitive.Bool (PBool)
import Plutarch.Primitive.Con (PCon (pcon'))
import Plutarch.Primitive.Eq (PEq (peq))
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Match (PMatch (pmatch'))

-- | @since wip
deriveMS :: Vector (TyVarBndr BndrVis) -> Name -> PTypeSum -> Q [Dec]
deriveMS tvbs name (PTypeSum typeStructure) = case NEVector.unsnoc typeStructure of
  (ps, (nameLast, PTypeProduct fieldsLast)) -> do
    when
      (NEVector.all (Vector.null . coerce @_ @(Vector PType) . snd) typeStructure)
      (fail "Use the Enum strategy for types with no fields in any 'arm'.")
    markedFieldsLast <- traverse (checkAndMark name) fieldsLast
    markedRest <- traverse (\(name', PTypeProduct fields) -> (name',) <$> traverse (checkAndMark name) fields) ps
    plutarchTypeDec <- derivePlutarchType tvbs name
    pmatchDec <- derivePMatch tvbs name markedRest (nameLast, markedFieldsLast)
    let markedAll = NEVector.snocV markedRest (nameLast, markedFieldsLast)
    pconDec <- derivePCon tvbs name markedAll
    peqDec <- derivePEq tvbs name markedAll
    pure $ plutarchTypeDec <> pmatchDec <> pconDec <> peqDec

-- Helpers

derivePlutarchType :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePlutarchType tyVars tyName =
  [d|
    instance $ctx => PlutarchType $name where
      type PRepresentation $name = $generalizedName
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PlutarchType $ tyVars
    generalizedName :: Q Type
    generalizedName =
      pure
        . foldl' (\acc v -> AppT acc (AppT (ConT ''PRepresentation) . VarT . bindToName $ v)) (ConT tyName)
        $ tyVars

derivePMatch ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  Vector (Name, Vector (Bool, Type)) ->
  (Name, Vector (Bool, Type)) ->
  Q [Dec]
derivePMatch tyVars tyName cs c =
  [d|
    instance $ctx => PMatch $name where
      pmatch' ::
        forall (b :: S -> GHC.Type) (s :: S).
        Term s (PRepresentation $name) -> ($name s -> Term s b) -> Term s b
      pmatch' x f = $(mkPMatchBody 'x 'f ''b)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PlutarchType $ tyVars
    mkPMatchBody :: Name -> Name -> Name -> Q Exp
    mkPMatchBody msThingName contName resTyName = do
      msType <- mkMSType (VarT resTyName) (Vector.snoc cs c)
      msName <- newName "asMS"
      handlerLast <- mkHandler contName c
      handlersRest <- traverse (mkHandler contName) cs
      let handlers = Vector.snoc handlersRest handlerLast
      bodyAppE <- foldlM (\acc e -> [e|papp $(pure acc) $(pure e)|]) (VarE msName) handlers
      let coerceE = AppE (AppTypeE (AppTypeE (VarE 'punsafeCoerce) WildCardT) msType) . VarE $ msThingName
      pure . LetE [ValD (VarP msName) (NormalB coerceE) []] $ bodyAppE
    mkHandler :: Name -> (Name, Vector (Bool, Type)) -> Q Exp
    mkHandler contName (cName, fields) = do
      let cArity = Vector.length fields
      case cArity of
        0 -> [e|$(pure . VarE $ contName) $(pure . VarE $ cName)|]
        _ -> go contName cName [] cArity
    go :: Name -> Name -> [Name] -> Int -> Q Exp
    go contName cName bindsNames = \case
      0 -> do
        let conAppE = foldr (\bindName acc -> AppE acc (VarE bindName)) (ConE cName) bindsNames
        pure . AppE (VarE contName) $ conAppE
      n -> mkPLam $ \bindName ->
        go contName cName (bindName : bindsNames) (n - 1)

derivePCon ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  NonEmptyVector (Name, Vector (Bool, Type)) ->
  Q [Dec]
derivePCon tyVars tyName constructors =
  [d|
    instance $ctx => PCon $name where
      pcon' x = punsafeCoerce $(cases 'x)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PlutarchType $ tyVars
    cases :: Name -> Q Exp
    cases argName = CaseE (VarE argName) . NEVector.toList <$> NEVector.imapM mkMatch constructors
    mkMatch :: Int -> (Name, Vector (Bool, Type)) -> Q Match
    mkMatch conIx (conName, fields) = do
      fieldNames <- itraverse (\i _ -> newName $ "f" <> show i) fields
      let conMatchPat = ConP conName [] . Vector.toList . fmap VarP $ fieldNames
      handlerName <- newName "handler"
      let conCount = NEVector.length constructors
      let matchBody = go handlerName conIx fieldNames conCount 0
      pure . Match conMatchPat (NormalB matchBody) $ []
    go :: Name -> Int -> Vector Name -> Int -> Int -> Exp
    go handlerName conIx fieldNames conCount depth
      | depth == conCount = foldl' (\acc -> AppE (AppE (VarE 'papp) acc) . VarE) (VarE handlerName) fieldNames
      | depth == conIx = AppE (VarE 'plam') . LamE [VarP handlerName] . go handlerName conIx fieldNames conCount $ depth + 1
      | otherwise = AppE (VarE 'plam') . LamE [WildP] . go handlerName conIx fieldNames conCount $ depth + 1

derivePEq ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  NonEmptyVector (Name, Vector (Bool, Type)) ->
  Q [Dec]
derivePEq tyVars tyName constructors =
  if NEVector.any (\(_, fields) -> Vector.any fst fields) constructors
    -- If we're isorecursive, we have to make sure we have a fixpoint with `self`
    -- available for the recursive fields.
    then
      [d|
        instance $ctx => PEq $name where
          peq = pfix $ \self -> $plam'E $ \x -> $plam'E $ \y ->
            $(peqImpl 'self 'x 'y)
        |]
    -- For uniformity, we use _one_ generator, and just pass `peq` as the `self`
    -- in cases where the type is 'flat'.
    else
      [d|
        instance $ctx => PEq $name where
          peq = $plam'E $ \x -> $plam'E $ \y ->
            $(peqImpl 'peq 'x 'y)
        |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PEq $ tyVars
    peqImpl :: Name -> Name -> Name -> Q Exp
    peqImpl selfName xName yName = do
      msType <- mkMSType (ConT ''PBool) (NEVector.toVector constructors)
      msNameX <- newName "msX"
      msNameY <- newName "msY"
      let coerceXE = AppE (AppTypeE (AppTypeE (VarE 'punsafeCoerce) WildCardT) msType) . VarE $ xName
      let coerceYE = AppE (AppTypeE (AppTypeE (VarE 'punsafeCoerce) WildCardT) msType) . VarE $ yName
      outerPlams <- NEVector.imapM (mkOuterPlam selfName msNameY) constructors
      bodyAppE <- foldlM (\acc e -> [e|$(pure acc) # $(pure e)|]) (VarE msNameX) outerPlams
      let letBinds =
            [ ValD (VarP msNameX) (NormalB coerceXE) []
            , ValD (VarP msNameY) (NormalB coerceYE) []
            ]
      pure . LetE letBinds $ bodyAppE
    mkOuterPlam :: Name -> Name -> Int -> (Name, Vector (Bool, Type)) -> Q Exp
    mkOuterPlam selfName msNameY argPos (_, fields) = do
      let cArity = Vector.length fields
      go selfName msNameY argPos [] cArity
    go :: Name -> Name -> Int -> [Name] -> Int -> Q Exp
    go selfName msNameY argPos xArgNames = \case
      0 -> mkInnerCall selfName msNameY argPos xArgNames
      n -> mkPLam $ \xArgName -> go selfName msNameY argPos (xArgName : xArgNames) (n - 1)
    mkInnerCall :: Name -> Name -> Int -> [Name] -> Q Exp
    mkInnerCall selfName msNameY argPos xArgNames = do
      innerPlams <- NEVector.imapM (mkInnerPlam selfName argPos xArgNames) constructors
      foldlM (\acc e -> [e|$(pure acc) # $(pure e)|]) (VarE msNameY) innerPlams
    mkInnerPlam :: Name -> Int -> [Name] -> Int -> (Name, Vector (Bool, Type)) -> Q Exp
    mkInnerPlam selfName outerArgPos xArgNames innerArgPos (_, fields) =
      let cArity = Vector.length fields
       in if outerArgPos == innerArgPos
            then goNamed (VarE selfName) fields xArgNames [] cArity
            else goNameless cArity
    goNamed :: Exp -> Vector (Bool, Type) -> [Name] -> [Name] -> Int -> Q Exp
    goNamed selfE fields xArgNames yArgNames = \case
      0 -> do
        -- Because `xArgNames` and `yArgNames` are accumulated in _reverse_ binding
        -- order, to 'line them up' with `fields`, we have to both zip _and_
        -- reverse them.
        let argsAsVec = Vector.fromList . reverse . zip xArgNames $ yArgNames
        let combinedVec = Vector.zipWith (\(x, y) (isRec, _) -> (x, y, isRec)) argsAsVec fields
        case Vector.uncons combinedVec of
          Nothing -> [e|ptrue|]
          Just ((x1, y1, isRec1), args) -> do
            start <-
              if isRec1
                then [e|$(pure selfE) # $(pure (VarE x1)) # $(pure (VarE y1))|]
                else [e|peq # $(pure (VarE x1)) # $(pure (VarE y1))|]
            foldlM (step selfE) start args
      n -> mkPLam $ \yArgName -> goNamed selfE fields xArgNames (yArgName : yArgNames) (n - 1)
    goNameless :: Int -> Q Exp
    goNameless = \case
      0 -> [e|pfalse|]
      n -> mkAnonPLam (goNameless $ n - 1)
    step :: Exp -> Exp -> (Name, Name, Bool) -> Q Exp
    step selfE acc (xName, yName, isRec) =
      if isRec
        then [e|pand $(pure acc) ($(pure selfE) # $(pure (VarE xName)) # $(pure (VarE yName)))|]
        else [e|pand $(pure acc) (peq # $(pure (VarE xName)) # $(pure (VarE yName)))|]

mkMSType :: Type -> Vector (Name, Vector (Bool, Type)) -> Q Type
mkMSType resType cs = do
  allFunTypes <- traverse (mkConFunType resType) cs
  foldrM mkArrow resType allFunTypes

mkConFunType :: Type -> (Name, Vector (Bool, Type)) -> Q Type
mkConFunType rType (_, fields) = foldrM mkArrow rType . fmap snd $ fields

mkArrow :: Type -> Type -> Q Type
mkArrow t acc = [t|$(pure t) :--> $(pure acc)|]
