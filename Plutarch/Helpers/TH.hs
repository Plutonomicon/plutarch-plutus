{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.Helpers.TH (
  checkTyName,
  bindToName,
  fullTypeName,
  mkContextOf,
  mkUncons,
  toSomeTermE,
  punsafeCaseE,
  punsafeConstrE,
  punsafeConstantE,
  pequalsIntegerE,
  plam'E,
  punsafeCoerceE,
  punConstrDataE,
  pnilDataE,
  pmkConsE,
  pconstrDataE,
  pequalsDataE,
  mkPLam,
  mkAnonPLam,
  PType (..),
  PTypeSum (..),
  PTypeProduct (..),
  consToPTypeSum,
  unwrapField,
  isTypeRecursive,
  checkAndMark,
) where

import Control.Monad (unless)
import Data.Foldable (foldl')
import Data.Kind qualified as GHC
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Language.Haskell.TH (
  Bang,
  BndrVis,
  Con (ForallC, GadtC, InfixC, NormalC, RecC, RecGadtC),
  Dec,
  Exp (AppE, LamE, VarE),
  Info (TyConI),
  Name,
  Pat (VarP, WildP),
  Q,
  TyVarBndr (KindedTV, PlainTV),
  Type (AppT, ConT, InfixT, TupleT, VarT),
  newName,
  reify,
 )
import Plutarch.Backend.Term (
  Term,
  plam',
  punsafeCase,
  punsafeCoerce,
  punsafeConstant,
  punsafeConstr,
  toSomeTerm,
 )
import Plutarch.Primitive.BuiltinFun (
  pconstrData,
  pequalsData,
  pequalsInteger,
  pmkCons,
  pnilData,
  punConstrData,
 )
import Plutarch.Primitive.Data (PAsData)
import Plutarch.Primitive.Function ((:-->))

{- | Return the declaration of a type given its name, or error out if the name
does not name a type.

@since wip
-}
checkTyName :: Name -> Q Dec
checkTyName tyName = do
  tyInfo <- reify tyName
  case tyInfo of
    TyConI tyDec -> pure tyDec
    _ -> fail $ show tyName <> " does not name a type."

{- | Given a binder, yield the name it binds.

@since wip
-}
bindToName :: TyVarBndr BndrVis -> Name
bindToName = \case
  PlainTV n _ -> n
  KindedTV n _ _ -> n

{- | Given a \'base\' type name, and a list of type variable binds, construct
the type name with all those binds applied.

@since wip
-}
fullTypeName ::
  forall (f :: GHC.Type -> GHC.Type).
  Foldable f => Name -> f (TyVarBndr BndrVis) -> Type
fullTypeName tyName = foldl' (\acc -> AppT acc . VarT . bindToName) (ConT tyName)

{- | Given a 'Vector' of type variables, and the name of a single-parameter type
class, construct a context asserting all of these type variables are
instances of that type class.

@since wip
-}
mkContextOf :: Name -> Vector (TyVarBndr BndrVis) -> Type
mkContextOf tyClassName tyVars =
  let len = Vector.length tyVars
      varNames = fmap (AppT (ConT tyClassName) . VarT . bindToName) tyVars
   in foldl' AppT (TupleT len) varNames

{- | Given a name @ell@ corresponding to a @PDList@ variable, constructs the
equivalent of:

@
punsafeCase ell . NEVector.singleton . toSomeTerm $ f headName tailName
@

@headName@ and @tailName@ are passed to the continuation (being generated
locally).

@since wip
-}
mkUncons :: Name -> (Name -> Name -> Q Exp) -> Q Exp
mkUncons listName f = do
  hName <- newName "h"
  tName <- newName "t"
  body <- f hName tName
  let innerLam = AppE (VarE 'plam') . LamE [VarP tName] $ body
  let outerLam = AppE (VarE 'plam') . LamE [VarP hName] $ innerLam
  [e|punsafeCase $(pure (VarE listName)) (NEVector.singleton (toSomeTerm $(pure outerLam)))|]

-- | @since wip
toSomeTermE :: Q Exp
toSomeTermE = pure $ VarE 'toSomeTerm

-- | @since wip
punsafeCaseE :: Q Exp
punsafeCaseE = pure $ VarE 'punsafeCase

-- | @since wip
punsafeConstrE :: Q Exp
punsafeConstrE = pure $ VarE 'punsafeConstr

-- | @since wip
punsafeConstantE :: Q Exp
punsafeConstantE = pure $ VarE 'punsafeConstant

-- | @since wip
pequalsIntegerE :: Q Exp
pequalsIntegerE = pure $ VarE 'pequalsInteger

-- | @since wip
plam'E :: Q Exp
plam'E = pure $ VarE 'plam'

-- | @since wip
punsafeCoerceE :: Q Exp
punsafeCoerceE = pure $ VarE 'punsafeCoerce

-- | @since wip
punConstrDataE :: Q Exp
punConstrDataE = pure $ VarE 'punConstrData

-- | @since wip
pnilDataE :: Q Exp
pnilDataE = pure $ VarE 'pnilData

-- | @since wip
pmkConsE :: Q Exp
pmkConsE = pure $ VarE 'pmkCons

-- | @since wip
pconstrDataE :: Q Exp
pconstrDataE = pure $ VarE 'pconstrData

-- | @since wip
pequalsDataE :: Q Exp
pequalsDataE = pure $ VarE 'pequalsData

{- | Given a \'handler\' taking the named argument, constructs a new @plam'@
with a fresh argument.

@since wip
-}
mkPLam :: (Name -> Q Exp) -> Q Exp
mkPLam f = do
  argName <- newName "x"
  body <- f argName
  pure . AppE (VarE 'plam') . LamE [VarP argName] $ body

{- | As 'mkPLam', but does not name a fresh argument.

@since wip
-}
mkAnonPLam :: Q Exp -> Q Exp
mkAnonPLam f = AppE (VarE 'plam') . LamE [WildP] <$> f

{- | A \'classifier type\' for Plutarch types.

@since wip
-}
data PType
  = -- | 'PAsData' wrapped.
    PTypeData Type
  | -- | A function type.
    PTypeFunction Type
  | -- | Something else.
    PTypeNotData Type

{- | A sum of 'PType' products.

@since wip
-}
newtype PTypeSum = PTypeSum (NonEmptyVector (Name, PTypeProduct))

{- | A 'PType' product.

@since wip
-}
newtype PTypeProduct = PTypeProduct (Vector PType)

{- | Given a 'Vector' of constructors, produce the corresponding 'PTypeSum'.
Errors if given an empty 'Vector'.

@since wip
-}
consToPTypeSum :: Vector Con -> Q PTypeSum
consToPTypeSum v = case Vector.uncons v of
  Nothing -> fail "Cannot derive for nullary types."
  Just (c, cs) -> PTypeSum <$> traverse conToPTypeProduct (NEVector.consV c cs)

{- | Produce the \'inner type\' (removing the 'PAsData' wrapper) for a 'PType'
that represents @'PAsData' t@ for some @t@, erroring otherwise.

@since wip
-}
unwrapField :: PType -> Q Type
unwrapField = \case
  PTypeData t -> pure t
  _ -> fail "Cannot use this derivation if a field is not wrapped in 'PAsData'."

{- | Verify whether a given 'PType' is recursive with the given 'Name' or not.

@since wip
-}
isTypeRecursive :: Name -> PType -> Bool
isTypeRecursive name = \case
  PTypeData t -> go t
  PTypeNotData t -> go t
  PTypeFunction t -> go t
  where
    go :: Type -> Bool
    go = \case
      ConT name' -> name' == name
      AppT x y -> go x || go y
      InfixT t name' u -> name' == name || go t || go u
      _ -> False

{- | Verify that a given 'PType' is not a function type (erroring if so), and
produces both of the following:

* An indication of whether the type is recursive given the 'Name' argument
('True' means \'recursive\'); and
* The 'Type' as it would be originally (restoring 'PAsData' if it was there).

@since wip
-}
checkAndMark :: Name -> PType -> Q (Bool, Type)
checkAndMark name t = case t of
  PTypeFunction _ -> fail "Functions cannot have derivations for PEq."
  PTypeData t' -> pure (isTypeRecursive name t, AppT (ConT ''PAsData) t')
  PTypeNotData t' -> pure (isTypeRecursive name t, t')

-- Helpers

conToPTypeProduct :: Con -> Q (Name, PTypeProduct)
conToPTypeProduct = \case
  NormalC name fields -> addName name <$> (traverse (go name) . Vector.fromList $ fields)
  RecC name fields -> addName name <$> (traverse (goNamed name) . Vector.fromList $ fields)
  InfixC lhs name rhs -> do
    let pieces = Vector.fromListN 2 [lhs, rhs]
    addName name <$> traverse (go name) pieces
  ForallC {} -> fail "Derivation does not work on nested foralls."
  GadtC {} -> fail "Derivation does not work on GADTs."
  RecGadtC {} -> fail "Derivation does not work on GADTs."
  where
    addName :: Name -> Vector PType -> (Name, PTypeProduct)
    addName name p = (name, PTypeProduct p)
    go :: Name -> (Bang, Type) -> Q PType
    go conName (_, t) =
      let errMsg = "Constructor " <> show conName <> " has a field whose type is not wrapped in 'Term'."
       in dig errMsg t
    goNamed :: Name -> (Name, Bang, Type) -> Q PType
    goNamed conName (fieldName, _, t) =
      let errMsg = "Constructor " <> show conName <> " has a field " <> show fieldName <> " whose type is not wrapped in 'Term'."
       in dig errMsg t
    dig :: String -> Type -> Q PType
    dig errMsg = \case
      AppT (AppT (ConT n) _) x -> do
        unless (n == ''Term) (fail errMsg)
        classifyType x
      _ -> fail errMsg

classifyType :: Type -> Q PType
classifyType t = case t of
  AppT (ConT t') x ->
    if t' == ''PAsData
      then
        if isTypePFunction x
          then fail "PAsData wrapping a Plutarch function makes no sense."
          else pure $ PTypeData x
      else pure $ PTypeNotData t
  _ ->
    pure $
      if isTypePFunction t
        then PTypeFunction t
        else PTypeNotData t

isTypePFunction :: Type -> Bool
isTypePFunction = \case
  InfixT _ name _ -> name == ''(:-->)
  _ -> False
