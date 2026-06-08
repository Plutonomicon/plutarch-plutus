{-# LANGUAGE NoPartialTypeSignatures #-}

{- | Helpers for building up UPLC @Term@s.

@since wip
-}
module Plutarch.Backend.UPLC (
  UPLCTerm (..),
  uplcApply,
  uplcApply1,
  uplcLam,
  uplcLam1,
  uplcLet,
  uplcForce,
  uplcDelay,
  uplcVar,
  uplcConstant,
  uplcBuiltin,
  uplcError,
  uplcConstr,
  uplcCase,
  uplcMCombinator,
) where

import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Data.Word (Word64)
import PlutusCore (Some, ValueOf)
import UntypedPlutusCore qualified as UPLC

{- | A wrapper around the polymorphism soup that is the UPLC @Term@. This is
both simpler and more concise to work with, and also allows us to provide our
own helpers.

@since wip
-}
newtype UPLCTerm = UPLCTerm (UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ())
  deriving
    ( -- | @since wip
      Eq
    , -- | @since wip
      Show
    , -- | @since wip
      Hashable
    )
    via (UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ())

{- | The @M@ combinator, or @\x -> x@. The argument provides the name of its
variable (in this case, @x@).

@since wip
-}
uplcMCombinator :: UPLC.Name -> UPLCTerm
uplcMCombinator arg = uplcLam1 arg . uplcApply1 (uplcVar arg) . uplcVar $ arg

{- | @'uplcLet' name v f@ produces the equivalent of @(\name -> f) v@.

@since wip
-}
uplcLet :: UPLC.Name -> UPLCTerm -> UPLCTerm -> UPLCTerm
uplcLet varName v f = uplcApply1 (uplcLam1 varName f) v

{- | Arity-1 application.

@since wip
-}
uplcApply1 :: UPLCTerm -> UPLCTerm -> UPLCTerm
uplcApply1 (UPLCTerm f) (UPLCTerm x) = UPLCTerm . UPLC.Apply () f $ x

{- | Arbitrary-arity application.

@since wip
-}
uplcApply :: UPLCTerm -> NonEmptyVector UPLCTerm -> UPLCTerm
uplcApply = NEVector.foldl' uplcApply1

{- | The @force@ from UPLC.

@since wip
-}
uplcForce :: UPLCTerm -> UPLCTerm
uplcForce (UPLCTerm x) = UPLCTerm . UPLC.Force () $ x

{- | The @delay@ from UPLC.

@since wip
-}
uplcDelay :: UPLCTerm -> UPLCTerm
uplcDelay (UPLCTerm x) = UPLCTerm . UPLC.Delay () $ x

{- | A variable with the given name.

@since wip
-}
uplcVar :: UPLC.Name -> UPLCTerm
uplcVar = UPLCTerm . UPLC.Var ()

{- | The given constant.

@since wip
-}
uplcConstant :: Some (ValueOf UPLC.DefaultUni) -> UPLCTerm
uplcConstant = UPLCTerm . UPLC.Constant ()

{- | The given builtin.

@since wip
-}
uplcBuiltin :: UPLC.DefaultFun -> UPLCTerm
uplcBuiltin = UPLCTerm . UPLC.Builtin ()

{- | The canonical error term.

@since wip
-}
uplcError :: UPLCTerm
uplcError = UPLCTerm . UPLC.Error $ ()

{- | A UPLC @constr@.

@since wip
-}
uplcConstr :: Word64 -> Vector UPLCTerm -> UPLCTerm
uplcConstr tag = UPLCTerm . UPLC.Constr () tag . Vector.toList . fmap coerce

{- | A UPLC @case@.

@since wip
-}
uplcCase :: UPLCTerm -> NonEmptyVector UPLCTerm -> UPLCTerm
uplcCase (UPLCTerm scrut) = UPLCTerm . UPLC.Case () scrut . NEVector.toVector . fmap coerce

{- | A lambda of arbitrary arity.

@since wip
-}
uplcLam :: NonEmptyVector UPLC.Name -> UPLCTerm -> UPLCTerm
uplcLam argNames body = NEVector.foldr uplcLam1 body argNames

{- | A lambda of specifically arity 1.

@since wip
-}
uplcLam1 :: UPLC.Name -> UPLCTerm -> UPLCTerm
uplcLam1 varName (UPLCTerm body) = UPLCTerm . UPLC.LamAbs () varName $ body
