{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Plutarch.Test.Utils (
  fewerTests,
  prettyShow,
  prettyEquals,
  typeName,
  instanceOfType,
  typeName',
  precompileTerm,
) where

import Data.Kind (Type)
import Plutarch.Internal.Term (
  Config (NoTracing),
  RawTerm (RCompiled),
  Term (Term),
  TermResult (TermResult),
  compile,
 )
import Plutarch.Prelude
import Plutarch.Script (Script (Script))
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutPretty, (<+>))
import Prettyprinter.Render.String (renderString)
import Test.Tasty.QuickCheck (Property, QuickCheckTests, counterexample)
import Type.Reflection (TypeRep, Typeable, tyConName, typeRep, typeRepTyCon, pattern App)
import UntypedPlutusCore (Program (_progTerm))

-- | Decrease number of quickcheck tests by specified factor
fewerTests :: QuickCheckTests -> QuickCheckTests -> QuickCheckTests
fewerTests divisor = (`quot` divisor)

prettyShow :: forall (a :: Type). Pretty a => a -> String
prettyShow = renderString . layoutPretty defaultLayoutOptions . pretty

prettyEquals :: (Eq a, Pretty a) => a -> a -> Property
prettyEquals x y =
  counterexample
    (renderString $ layoutPretty defaultLayoutOptions (pretty x <+> interpret res <+> pretty y))
    res
  where
    res = x == y
    interpret True = " == "
    interpret False = " /= "

typeName :: forall k (a :: k). Typeable a => String
typeName = typeName' True (typeRep @a)

typeName' ::
  -- | Wrap in parentheses if contains space
  Bool ->
  TypeRep k ->
  String
typeName' isTopLevel rep =
  case rep of
    App lhs rhs -> wrap (typeName' False lhs <> " " <> typeName' False rhs)
    rep -> tyConName $ typeRepTyCon rep
  where
    wrap :: String -> String
    wrap s
      | not isTopLevel = "(" <> s <> ")"
      | otherwise = s

instanceOfType ::
  forall k (a :: k).
  Typeable a =>
  -- | Instance name
  String ->
  String
instanceOfType instanceName = instanceName <> " " <> typeName' False (typeRep @a)

-- | @since 1.0.0
precompileTerm :: forall (p :: S -> Type). ClosedTerm p -> ClosedTerm p
precompileTerm t =
  case compile NoTracing t of
    Left err -> error $ "precompileTerm: failed to compile: " <> show err
    Right script -> unsafeTermFromScript script

unsafeTermFromScript :: forall (p :: S -> Type). Script -> ClosedTerm p
unsafeTermFromScript (Script script) =
  Term $ const $ pure $ TermResult (RCompiled $ _progTerm script) []
