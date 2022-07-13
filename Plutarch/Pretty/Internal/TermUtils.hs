{-# LANGUAGE PatternSynonyms #-}

module Plutarch.Pretty.Internal.TermUtils (
  unwrapLamAbs,
  unwrapBindings,
  unwrapApply,
  incrVar,
  pattern PFixAst,
  pattern ComposeAST,
  pattern IfThenElseLikeAST,
) where

import UntypedPlutusCore (
  DeBruijn (DeBruijn),
  Index,
  Term (Apply, Delay, Force, LamAbs, Var),
 )

unwrapLamAbs :: Index -> Term name uni fun ann -> (Index, Term name uni fun ann)
unwrapLamAbs d (LamAbs _ _ t) = unwrapLamAbs (d + 1) t
unwrapLamAbs d a = (d, a)

unwrapBindings :: [Term name uni fun ann] -> Term name uni fun ann -> ([Term name uni fun ann], Term name uni fun ann)
unwrapBindings l (Apply _ (LamAbs _ _ t) arg) = unwrapBindings (arg : l) t
unwrapBindings l a = (l, a)

unwrapApply ::
  [Term name uni fun ann] ->
  Term name uni fun ann ->
  ([Term name uni fun ann], Term name uni fun ann)
unwrapApply l (Apply _ t arg) = unwrapApply (arg : l) t
unwrapApply l arg = (l, arg)

-- AST resulting from `pfix`. This is always constant.
pattern PFixAst :: Term name uni fun ()
pattern PFixAst <-
  LamAbs
    ()
    _
    ( Apply
        ()
        ( LamAbs
            ()
            _
            ( Apply
                ()
                (Var () _)
                ( LamAbs
                    ()
                    _
                    ( Apply
                        ()
                        ( Apply
                            ()
                            (Var () _)
                            (Var () _)
                          )
                        (Var () _)
                      )
                  )
              )
          )
        ( LamAbs
            ()
            _
            ( Apply
                ()
                (Var () _)
                ( LamAbs
                    ()
                    _
                    ( Apply
                        ()
                        ( Apply
                            ()
                            (Var () _)
                            (Var () _)
                          )
                        (Var () _)
                      )
                  )
              )
          )
      )

-- If `f` and `g` are Var references, their indices are incremented once since they are within a lambda.
pattern ComposeAST :: Term DeBruijn uni fun () -> Term DeBruijn uni fun () -> Term DeBruijn uni fun ()
pattern ComposeAST f g <- LamAbs () _ (Apply () (incrVar -> f) (Apply () (incrVar -> g) (Var () (DeBruijn 1))))

{- This AST represents a typical if/then/else usage if and only if 'ifThenElseMaybe' is either the
builtin IfThenElse (forced once), or a reference to such.
-}
pattern IfThenElseLikeAST ::
  Term name uni fun () ->
  Term name uni fun () ->
  Term name uni fun () ->
  Term name uni fun () ->
  Term name uni fun ()
pattern IfThenElseLikeAST ifThenElseMaybe cond trueBranch falseBranch <-
  Force
    ()
    ( Apply
        ()
        ( Apply
            ()
            ( Apply
                ()
                ifThenElseMaybe
                cond
              )
            (Delay () trueBranch)
          )
        (Delay () falseBranch)
      )

-- | Increment the debruijn index of a 'Var', leave any other AST node unchanged.
incrVar :: Term DeBruijn uni fun () -> Term DeBruijn uni fun ()
incrVar (Var () (DeBruijn n)) = Var () . DeBruijn $ n - 1
incrVar n = n
