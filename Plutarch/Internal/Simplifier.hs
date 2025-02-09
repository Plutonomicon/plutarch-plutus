{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Plutarch.Internal.Simplifier (simplifier) where

import Data.Text (Text)
import Data.Word (Word64)
import Debug.Trace
import PlutusCore qualified as PLC
import PlutusCore.Builtin qualified as PLC
import PlutusCore.Error qualified as PLC
import Prettyprinter
import UntypedPlutusCore
import UntypedPlutusCore.Parser

applyTopLam ::
  Term DeBruijn DefaultUni DefaultFun () ->
  ([Integer], Term DeBruijn DefaultUni DefaultFun ()) ->
  ([Integer], Term DeBruijn DefaultUni DefaultFun ())
applyTopLam target (argUsed, arg) = subst' 0 target
  where
    incrementTerm :: Word64 -> Word64 -> Term DeBruijn DefaultUni DefaultFun () -> Term DeBruijn DefaultUni DefaultFun ()
    incrementTerm d incrAmount org@(Var () (DeBruijn (Index idx)))
      | idx <= d = org
      | otherwise = Var () (DeBruijn (Index $ idx + incrAmount))
    incrementTerm d incrAmount (LamAbs () v t) = LamAbs () v (incrementTerm (d + 1) incrAmount t)
    incrementTerm d incrAmount (Apply () f t) = Apply () (incrementTerm d incrAmount f) (incrementTerm d incrAmount t)
    incrementTerm d incrAmount (Force () t) = Force () (incrementTerm d incrAmount t)
    incrementTerm d incrAmount (Delay () t) = Delay () (incrementTerm d incrAmount t)
    incrementTerm d incrAmount (Constr () idx ts) = Constr () idx (incrementTerm d incrAmount <$> ts)
    incrementTerm d incrAmount (Case () t ts) = Case () (incrementTerm d incrAmount t) (incrementTerm d incrAmount <$> ts)
    incrementTerm _ _ x = x

    subst' :: Integer -> Term DeBruijn DefaultUni DefaultFun () -> ([Integer], Term DeBruijn DefaultUni DefaultFun ())
    subst' d x@(Var () (DeBruijn (Index (toInteger -> n)))) =
      case compare (n - d) 1 of
        EQ -> ((- d) <$> argUsed, incrementTerm 0 (fromInteger d) arg)
        LT -> ([1 - n], x)
        GT -> ([1 - (n - 1)], Var () (DeBruijn (Index $ fromInteger $ n - 1)))
    subst' d (LamAbs () v t) = let (used', t') = subst' (d + 1) t in ((+ 1) <$> used', LamAbs () v t')
    subst' d (Apply () f t) = Apply () <$> subst' d f <*> subst' d t
    subst' d (Force () t) = Force () <$> subst' d t
    subst' d (Delay () t) = Delay () <$> subst' d t
    subst' d (Constr () idx ts) = Constr () idx <$> traverse (subst' d) ts
    subst' d (Case () t ts) = Case () <$> subst' d t <*> traverse (subst' d) ts
    subst' _ x = (mempty, x)

simplifier :: Term DeBruijn DefaultUni DefaultFun () -> Term DeBruijn DefaultUni DefaultFun ()
simplifier = snd . go
  where
    go :: Term DeBruijn DefaultUni DefaultFun () -> ([Integer], Term DeBruijn DefaultUni DefaultFun ())
    go org@(Apply () (Apply () (Builtin () PLC.AddInteger) x) y) = do
      (,) <$> go x <*> go y >>= \case
        (x'@(Constant _ _), y'@(Constant _ _)) ->
          case (PLC.readKnownConstant x', PLC.readKnownConstant y') of
            (Right x'', Right y'') -> (mempty, Constant () $ PLC.someValue @Integer (x'' + y''))
            _ -> (mempty, org)
        (x', y') -> pure (Apply () (Apply () (Builtin () PLC.AddInteger) x') y')
    go org@(Apply () (Apply () (Builtin () PLC.EqualsInteger) x) y) =
      (,) <$> go x <*> go y >>= \case
        (x'@(Constant _ _), y'@(Constant _ _)) ->
          case (PLC.readKnownConstant @_ @Integer x', PLC.readKnownConstant @_ @Integer y') of
            (Right x'', Right y'') -> (mempty, Constant () $ PLC.someValue @Bool (x'' == y''))
            _ -> (mempty, org)
        (x', y') -> pure (Apply () (Apply () (Builtin () PLC.EqualsInteger) x') y')
    go (Apply () (Apply () (Apply () (Builtin () PLC.IfThenElse) cond@(Constant _ _)) x) y) =
      case PLC.readKnownConstant cond of
        Right cond' ->
          if cond'
            then go x
            else go y
        _ -> do
          x' <- go x
          y' <- go y
          pure (Apply () (Apply () (Apply () (Builtin () PLC.IfThenElse) cond) x') y')
    go (Apply () (LamAbs () v f) arg) =
      let
        (lamUsed, lamSimplified) = go f
        argBoth@(argUsed, argSimplified) = go arg
       in
        case argSimplified of
          c@(Constant _ _) ->
            let (_lamUsed, lamSimplified) = go f
             in applyTopLam lamSimplified (mempty, c)
          v@(Var _ (DeBruijn (Index i))) ->
            let (_lamUsed, lamSimplified) = go f
             in applyTopLam lamSimplified ([1 - toInteger i], v)
          _ ->
            if length (filter (== 0) lamUsed) <= 1
              then applyTopLam lamSimplified argBoth
              else (((+ 1) <$> lamUsed) <> argUsed, Apply () (LamAbs () v lamSimplified) argSimplified)
    go org@(Var () (DeBruijn (Index v))) = ([1 - toInteger v], org)
    go (LamAbs () v t) = let (used', t') = go t in ((+ 1) <$> used', LamAbs () v t')
    go (Apply () f t) =
      let
        (usedf, f') = go f
        (usedt, t') = go t
       in
        case f' of
          (LamAbs {}) -> go (Apply () f' t')
          _ -> (usedf <> usedt, Apply () f' t')
    go (Force () t) = Force () <$> go t
    go (Delay () t) = Delay () <$> go t
    go org@(Constant _ _) = (mempty, org)
    go org@(Builtin _ _) = (mempty, org)
    go org@(Error ()) = (mempty, org)
    go (Constr () idx ts) = Constr () idx <$> traverse go ts
    go (Case () t ts) = Case () <$> go t <*> traverse go ts
