{-# LANGUAGE TemplateHaskellQuotes #-}

module Plutarch.TH.Strategy (
  Strategy (..),
  deriveFor,
) where

import Data.Foldable (foldl', foldrM, for_)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty qualified as NEVector
import Language.Haskell.TH (
  Bang,
  BndrVis,
  Con (ForallC, GadtC, InfixC, NormalC, RecC, RecGadtC),
  Dec (DataD, NewtypeD, TySynD),
  Exp (AppE, ConE, LamE, VarE),
  Info (TyConI),
  Name,
  Pat (VarP),
  Q,
  TyVarBndr (KindedTV, PlainTV),
  Type (AppT, ConT, TupleT, VarT),
  newName,
  reify,
 )
import Plutarch.Backend.Term (Term, plam')
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))
import Plutarch.Primitive.Match (PMatch (pmatch'))
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
        let consAsVec = Vector.fromList constructors
        for_ consAsVec checkFieldsAreTermsSOP
        case Vector.unsnoc tvbAsVec of
          Nothing -> fail "Types must have an s :: S type parameter in last position."
          Just (tvbs, _) -> case Vector.unsnoc consAsVec of
            Nothing -> fail "Nullary types do not support an SOP derivation strategy."
            Just (cs, c) ->
              if all hasNoFields constructors
                then fail "Use the Enum strategy for types with no fields in any 'arm'."
                else do
                  plutarchTypeDec <- deriveSOPPlutarchType tvbs name
                  pmatchDec <- deriveSOPPMatch tvbs name cs c
                  pure $ plutarchTypeDec <> pmatchDec
    NewtypeD {} -> case strat of
      SOP -> fail "Use the Newtype strategy for newtypes."
    TySynD {} -> fail "Type synonyms are not supported. Use the underlying type."
    _ -> fail "Not a valid type name."

-- Helpers

deriveSOPPlutarchType :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
deriveSOPPlutarchType tyVars tyName =
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

deriveSOPPMatch :: Vector (TyVarBndr BndrVis) -> Name -> Vector Con -> Con -> Q [Dec]
deriveSOPPMatch tyVars tyName cs c =
  [d|
    instance $ctx => PMatch $name where
      pmatch' x f = let handlers = $(mkHandlers 'f) in punsafeCase x handlers
    |]
  where
    name :: Q Type
    name = pure $ foldl' (\acc -> AppT acc . VarT . bindToName) (ConT tyName) tyVars
    ctx :: Q Type
    ctx = do
      let len = Vector.length tyVars
      let varNames = fmap (AppT (ConT ''PlutarchType) . VarT . bindToName) tyVars
      pure $ foldl' AppT (TupleT len) varNames
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
      start <- [e|NEVector.singleton (toSomeTerm $(pure handlerLast))|]
      foldrM (\e acc -> [e|NEVector.cons (toSomeTerm $(pure e)) $(pure acc)|]) start handlersRest
    mkHandler :: Name -> (Name, Word) -> Q Exp
    mkHandler contName (conName, arity) = do
      argNames <- case arity of
        0 -> pure []
        n -> traverse (\i -> newName $ "x" <> show i) [0, 1 .. n - 1]
      let conCallExp = AppE (VarE contName) . foldl' (\acc -> AppE acc . VarE) (ConE conName) $ argNames
      pure . foldr (\name -> AppE (VarE 'plam') . LamE [VarP name]) conCallExp $ argNames

getArity :: Con -> Word
getArity = \case
  NormalC _ fields -> fromIntegral . length $ fields
  RecC _ fields -> fromIntegral . length $ fields
  InfixC {} -> 2
  ForallC _ _ con -> getArity con
  GadtC _ fields _ -> fromIntegral . length $ fields
  RecGadtC _ fields _ -> fromIntegral . length $ fields

conToName :: Con -> Q Name
conToName = \case
  NormalC name _ -> pure name
  RecC name _ -> pure name
  InfixC _ name _ -> pure name
  ForallC _ _ con -> conToName con
  GadtC {} -> fail "SOP derivation strategy does not work on GADTs."
  RecGadtC {} -> fail "SOP derivation strategy does not work on GADTs."

checkTyName :: Name -> Q Dec
checkTyName tyName = do
  tyInfo <- reify tyName
  case tyInfo of
    TyConI tyDec -> pure tyDec
    _ -> fail $ show tyName <> " does not name a type."

checkFieldsAreTermsSOP :: Con -> Q ()
checkFieldsAreTermsSOP = \case
  NormalC name fields -> for_ fields (checkFieldIsTerm name)
  RecC name fields -> for_ fields (checkNamedFieldIsTerm name)
  InfixC lhs name rhs -> do
    checkFieldIsTerm name lhs
    checkFieldIsTerm name rhs
  ForallC {} -> fail "SOP derivation strategy does not work on nested foralls."
  GadtC {} -> fail "SOP derivation strategy does not work on GADTs."
  RecGadtC {} -> fail "SOP derivation strategy does not work on GADTs."

checkFieldIsTerm :: Name -> (Bang, Type) -> Q ()
checkFieldIsTerm conName (_, t) = digForTerm t
  where
    digForTerm :: Type -> Q ()
    digForTerm = \case
      AppT x _ -> case x of
        ConT n ->
          if n == ''Term
            then pure ()
            else
              fail $
                "Constructor "
                  <> show conName
                  <> " has a field whose type is not wrapped in 'Term'."
        _ -> digForTerm x
      _ ->
        fail $
          "Constructor "
            <> show conName
            <> " has a field whose type is not wrapped in 'Term'."

checkNamedFieldIsTerm :: Name -> (Name, Bang, Type) -> Q ()
checkNamedFieldIsTerm conName (fieldName, _, t) = digForTerm t
  where
    digForTerm :: Type -> Q ()
    digForTerm = \case
      AppT x _ -> case x of
        ConT n ->
          if n == ''Term
            then pure ()
            else
              fail $
                "Constructor "
                  <> show conName
                  <> " has a field whose type is not wrapped in 'Term', specifically "
                  <> show fieldName
        _ -> digForTerm x
      _ ->
        fail $
          "Constructor "
            <> show conName
            <> " has a field whose type is not wrapped in 'Term', specifically "
            <> show fieldName

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
