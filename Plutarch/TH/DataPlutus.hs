{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.TH.DataPlutus (
  deriveDataPlutus,
) where

import Control.Monad (when)
import Data.Coerce (coerce)
import Data.Foldable (foldrM)
import Data.Traversable.WithIndex (itraverse)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Language.Haskell.TH (
  BndrVis,
  Body (NormalB),
  Dec,
  Exp (AppE, CaseE, ConE, LitE, VarE),
  Lit (IntegerL),
  Match (Match),
  Name,
  Pat (ConP, VarP),
  Q,
  TyVarBndr,
  Type,
  newName,
 )
import Plutarch.Helpers.TH (
  PType,
  PTypeProduct (PTypeProduct),
  PTypeSum (PTypeSum),
  fullTypeName,
  mkContextOf,
  mkUncons,
  pconstrDataE,
  pequalsDataE,
  plam'E,
  pmkConsE,
  pnilDataE,
  punConstrDataE,
  punsafeCaseE,
  punsafeCoerceE,
  punsafeConstantE,
  toSomeTermE,
  unwrapField,
 )
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))
import Plutarch.Primitive.BuiltinFun (pheadList)
import Plutarch.Primitive.CanData (PCanData)
import Plutarch.Primitive.Con (PCon (pcon'))
import Plutarch.Primitive.Data (PData)
import Plutarch.Primitive.Eq (PEq (peq))
import Plutarch.Primitive.Match (PMatch (pmatch'))
import Plutarch.Primitive.Pair (PBPair (PBPair))
import PlutusCore qualified as PLC

-- | @since wip
deriveDataPlutus :: Vector (TyVarBndr BndrVis) -> Name -> PTypeSum -> Q [Dec]
deriveDataPlutus tvbs name (PTypeSum typeStructure) = case NEVector.unsnoc typeStructure of
  (ps, (nameLast, PTypeProduct fieldsLast)) -> do
    when (Vector.null ps) (fail "Use the DataList strategy for a Data-encoded type with a single constructor.")
    when
      (NEVector.all (Vector.null . coerce @_ @(Vector PType) . snd) typeStructure)
      (fail "Use the Enum strategy for types with no fields in any 'arm.")
    unwrappedFieldsLast <- traverse unwrapField fieldsLast
    unwrappedPS <- traverse (\(name, PTypeProduct fields) -> (name,) <$> traverse unwrapField fields) ps
    plutarchTypeDec <- derivePlutarchType tvbs name
    pmatchDec <- derivePMatch tvbs name unwrappedPS (nameLast, unwrappedFieldsLast)
    pconDec <- derivePCon tvbs name (NEVector.snocV unwrappedPS (nameLast, unwrappedFieldsLast))
    peqDec <- derivePEq tvbs name
    pure $ plutarchTypeDec <> pmatchDec <> pconDec <> peqDec

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

derivePMatch ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  Vector (Name, Vector Type) ->
  (Name, Vector Type) ->
  Q [Dec]
derivePMatch tyVars tyName cs (nameLast, fieldsLast) =
  [d|
    instance $ctx => PMatch $name where
      pmatch' x f = pmatch ($punConstrDataE # x) $ \(PBPair tag fields) ->
        $(mkMatchBody 'f 'tag 'fields)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PCanData $ tyVars
    mkMatchBody :: Name -> Name -> Name -> Q Exp
    mkMatchBody contName tagName fieldsName = do
      let arityLast = Vector.length fieldsLast
      let namesRest = fmap fst cs
      let aritiesRest = fmap (Vector.length . snd) cs
      -- If we have a minimum required number of fields, we extract them _now_,
      -- to avoid having repeated code in our handlers doing the same thing.
      let minArity = Vector.foldl' min arityLast aritiesRest
      -- Offset all arities by the number we 'preload'
      let arityLast' = arityLast - minArity
      let aritiesRest' = fmap (- minArity) aritiesRest
      withPreload contName tagName (arityLast', nameLast) (aritiesRest', namesRest) [] fieldsName minArity
    -- Consider the following data type:
    --
    -- ```
    -- data PThese a b s =
    --     PThis (Term s (PAsData a)) |
    --     PThat (Term s (PAsData b)) |
    --     PThese (Term s (PAsData a)) (Term s (PAsData b))
    -- ```
    --
    -- The most straightforward @Data@ encoding would do something like the
    -- following:
    --
    -- \* `PThis t` becomes `Constr 0 [t]`
    -- \* `PThat t` becomes `Constr 1 [t]`
    -- \* `PThese t1 t2` becomes `Constr 2 [t1, t2]`
    --
    -- To 'take apart' such an encoding, we can see that we _must_ 'pull out'
    -- the first element of the 'field list' no matter which branch we take.
    -- Thus, an efficient sequence of events would be:
    --
    -- 1. Transform `PData` into `(PInteger, PBList PData)`
    -- 2. Take the head of the `PBList PData`
    -- 3. Branch on the tag; if we have `0` or `1`, assemble the `PThese`, if
    --    not, take the head _again_, then assemble the `PThese`.
    --
    -- However, if we were to just follow the constructor logic blindly, we
    -- would _instead_ get:
    --
    -- 1. Transform `PData` into `(PInteger, PBList PData)`
    -- 2. Branch on the tag; if we have `0`, take the head, then assemble, if we
    --    have `1`, take the head, then assemble, if we have 2, take the head
    --    twice, then assemble.
    --
    -- This forces us to duplicate the 'take the head' code into _every_ branch.
    --
    -- `withPreload` essentially ensures that in situations like the one above,
    -- the code that gets generated follows the efficient sequence of events
    -- above. More precisely, if _all_ 'arms' of a data type have a minimal
    -- number of fields, `withPreload` generates code to extract that number of
    -- fields _before_ branching on the tag.
    withPreload ::
      Name ->
      Name ->
      (Int, Name) ->
      (Vector Int, Vector Name) ->
      [Name] ->
      Name ->
      Int ->
      Q Exp
    withPreload contName tagName (arityLast, nameLast) (aritiesRest, namesRest) preloadNamesBackwards lastTail = \case
      0 -> do
        handlerLast <- mkHandler lastTail contName preloadNamesBackwards (nameLast, arityLast)
        handlersRest <- traverse (mkHandler lastTail contName preloadNamesBackwards) (Vector.zip namesRest aritiesRest)
        start <- [e|NEVector.singleton ($toSomeTermE $(pure handlerLast))|]
        handlers <- foldrM (\e acc -> [e|NEVector.cons ($toSomeTermE $(pure e)) $(pure acc)|]) start handlersRest
        [e|$punsafeCaseE $(pure (VarE tagName)) $(pure handlers)|]
      n -> mkUncons lastTail $ \headName tailName ->
        -- We accumulate all preloaded names so that we can apply them to the
        -- appropriate data constructor 'all at once' at the end.
        withPreload contName tagName (arityLast, nameLast) (aritiesRest, namesRest) (headName : preloadNamesBackwards) tailName (n - 1)
    mkHandler :: Name -> Name -> [Name] -> (Name, Int) -> Q Exp
    mkHandler lastTail contName preloadedNamesBackwards (conName, arity) = case arity of
      -- All of our arguments have been preloaded already.
      0 -> do
        let headsNames = reverse preloadedNamesBackwards
        conAppE <- foldrM (\headName acc -> AppE acc <$> [e|$punsafeCoerceE $(pure (VarE headName))|]) (ConE conName) headsNames
        pure . AppE (VarE contName) $ conAppE
      -- Some items still remain to be loaded.
      n -> go contName conName preloadedNamesBackwards lastTail (n - 1)
    go :: Name -> Name -> [Name] -> Name -> Int -> Q Exp
    go contName cName headsNamesBackwards lastTailName = \case
      0 -> do
        -- We accumulate the heads needed in reverse order, because otherwise,
        -- this is a quadratic procedure. We can reverse in linear time.
        let headsNames = reverse headsNamesBackwards
        -- Build up applications of all heads to the constructor.
        conAppButLast <- foldrM (\headName acc -> AppE acc <$> [e|$punsafeCoerceE $(pure (VarE headName))|]) (ConE cName) headsNames
        -- Add the last argument by taking the head of the last tail.
        conAppE <- AppE conAppButLast <$> [e|$punsafeCoerceE (pheadList @PData # $(pure (VarE lastTailName)))|]
        -- Hit it with the continuation internally.
        pure . AppE (VarE contName) $ conAppE
      n -> mkUncons lastTailName $ \headName tailName ->
        -- We accumulate the needed names of all the heads we have to take
        -- so that we can apply them to the constructor 'all at once' at
        -- the end.
        go contName cName (headName : headsNamesBackwards) tailName (n - 1)

derivePCon ::
  Vector (TyVarBndr BndrVis) ->
  Name ->
  NonEmptyVector (Name, Vector Type) ->
  Q [Dec]
derivePCon tyVars tyName constructors =
  [d|
    instance $ctx => PCon $name where
      pcon' x = $(matches 'x)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PCanData $ tyVars
    matches :: Name -> Q Exp
    matches bindName = CaseE (VarE bindName) . NEVector.toList <$> NEVector.imapM mkMatch constructors
    mkMatch :: Int -> (Name, Vector Type) -> Q Match
    mkMatch conIx (conName, conFields) = do
      fieldNames <- itraverse (\i _ -> newName $ "f" <> show i) conFields
      let conMatchPat = ConP conName [] . Vector.toList . fmap VarP $ fieldNames
      let constrIx = LitE . IntegerL . fromIntegral $ conIx
      constrE <- [e|$punsafeConstantE (PLC.someValue @Integer $(pure constrIx))|]
      start <- pnilDataE
      constrList <- foldrM go start fieldNames
      matchBody <- [e|$pconstrDataE # $(pure constrE) # $(pure constrList)|]
      pure . Match conMatchPat (NormalB matchBody) $ []
    go :: Name -> Exp -> Q Exp
    go fieldName acc = [e|$pmkConsE # pcoerce $(pure . VarE $ fieldName) # $(pure acc)|]

derivePEq :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePEq tyVars tyName =
  [d|
    instance $ctx => PEq $name where
      peq = $plam'E $ \x -> $plam'E $ \y -> $pequalsDataE # pcoerce x # pcoerce y
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PCanData $ tyVars
