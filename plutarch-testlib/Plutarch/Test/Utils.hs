{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Plutarch.Test.Utils (
  fewerTests,
  prettyShow,
  typeName,
  instanceOfType,
) where

import Data.Kind (Type)
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import Test.Tasty.QuickCheck (QuickCheckTests)
import Type.Reflection (TypeRep, Typeable, tyConName, typeRep, typeRepTyCon, pattern App)

-- | Decrease number of quickcheck tests by specified factor
fewerTests :: QuickCheckTests -> QuickCheckTests -> QuickCheckTests
fewerTests divisor = (`quot` divisor)

prettyShow :: forall (a :: Type). Pretty a => a -> String
prettyShow = renderString . layoutPretty defaultLayoutOptions . pretty

typeName :: forall k (a :: k). Typeable a => String
typeName = typeName' True (typeRep @a)

typeName' :: Bool -> TypeRep k -> String
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
