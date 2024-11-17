-- hehe
{-# OPTIONS_GHC -Wno-all #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}

module Plutarch.Fun where

import Control.Monad
import Data.List (intercalate)
import Data.RandomAccessList.Class qualified as RAL
import Data.Text qualified as Text
import Plutarch
import Plutarch.Prelude
import Plutarch.Script (Script (unScript))
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCekParametersForTesting)
import PlutusCore.Pretty qualified as Pretty
import PlutusCore.Quote
import Prettyprinter qualified as Pretty
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek
import UntypedPlutusCore.Evaluation.Machine.SteppableCek.Internal

import PlutusLedgerApi.V3

{-# ANN module "HLint: ignore" #-}

myAdd :: Term s (PInteger :--> PInteger :--> PInteger)
myAdd = plam $ \x y -> plet (x + y) (\x' -> x' + perror)

foo :: Term s PInteger
foo = pforce $ plam (\_x z -> z) # pdelay (pconstant $ Constr 10 [I 1, I 2]) #$ pdelay (pconstant @PInteger 20 + 20)

fun :: Script
fun =
  case compile mempty foo of
    Left _ -> error "no"
    Right x -> x

funner :: CekState UPLC.DefaultUni UPLC.DefaultFun ()
funner = Starting (UPLC.termMapNames UPLC.fakeNameDeBruijn $ UPLC._progTerm . unScript $ fun)

raToList :: (RAL.RandomAccessList e, a ~ RAL.Element e) => e -> [a]
raToList x =
  case RAL.uncons x of
    Nothing -> []
    Just (h, r) -> h : raToList r

composeM :: Monad m => Integer -> (a -> m a) -> (a -> m [a])
composeM 0 f x = do
  a <- f x
  pure [a]
composeM n f x = do
  curr <- f x
  rest <- composeM (n - 1) f curr

  pure $ curr : rest

funnerer ::
  ( Either
      (CekEvaluationException UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun)
      [CekState UPLC.DefaultUni UPLC.DefaultFun ()]
  , Cek.CountingSt
  , [Text.Text]
  )
funnerer =
  runCekM
    defaultCekParametersForTesting
    Cek.counting
    Cek.logEmitter
    $ (composeM 25 cekTrans) (Starting bar)
  where
    foo :: QuoteT (Either UPLC.FreeVariableError) (UPLC.Term UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun ())
    foo = UPLC.deBruijnTerm =<< UPLC.unDeBruijnTerm (UPLC.termMapNames UPLC.fakeNameDeBruijn $ UPLC._progTerm . unScript $ fun)
    bar =
      case runQuoteT foo of
        Right t -> t
        Left err -> error $ show err

{-
{ "type": "lam"

}
-}

prettyCekContext :: Context UPLC.DefaultUni UPLC.DefaultFun () -> Pretty.Doc ()
prettyCekContext (FrameAwaitArg _ val ctx) =
  "(Frame Awaiting Arg\n"
    <> ( Pretty.indent 2 $
          Pretty.vcat
            [ "Val:" Pretty.<+> Pretty.prettyBy (Pretty.prettyConfigPlcClassic Pretty.prettyConfigPlcOptions) val
            , prettyCekContext ctx
            ]
       )
    <> ")"
prettyCekContext (FrameAwaitFunTerm _ env term ctx) =
  "(Frame Await Function Term:\n"
    <> ( Pretty.indent 2 $
          Pretty.vcat
            [ "Env:" Pretty.<+> prettyCekEnv env
            , "Term:" Pretty.<+> Pretty.pretty term
            , prettyCekContext ctx
            ]
       )
    <> ")"
prettyCekContext (FrameAwaitFunValue _ val ctx) =
  "(Frame Awaiting Function Value:\n"
    <> ( Pretty.indent 2 $
          Pretty.vcat
            [ "Val: " Pretty.<+> Pretty.prettyBy (Pretty.prettyConfigPlcClassic Pretty.prettyConfigPlcOptions) val
            , prettyCekContext ctx
            ]
       )
    <> ")"
prettyCekContext (FrameForce _ ctx) =
  "(Frame Force:\n"
    <> ( Pretty.indent 2 $
          Pretty.vcat
            [ prettyCekContext ctx
            ]
       )
    <> ")"
prettyCekContext (FrameConstr _ val n terms argstack ctx) = undefined
prettyCekContext (FrameCases _ val terms ctx) = undefined
prettyCekContext NoFrame = "(NoFrame)"

prettyCekEnv :: CekValEnv UPLC.DefaultUni UPLC.DefaultFun ann -> Pretty.Doc ann
prettyCekEnv env =
  let
    envs :: [Pretty.Doc ann]
    envs =
      (Pretty.prettyBy (Pretty.prettyConfigPlcClassic Pretty.prettyConfigPlcOptions))
        <$> raToList env
   in
    Pretty.encloseSep "[" "]" "," envs

prettyCekState :: CekState UPLC.DefaultUni UPLC.DefaultFun () -> Pretty.Doc ()
prettyCekState (Starting t) =
  "Starting: " Pretty.<+> Pretty.pretty t
prettyCekState (Computing ctx env t) =
  "Computing:\n"
    <> ( Pretty.indent 2 $
          Pretty.vcat
            [ "Term:" Pretty.<+> Pretty.align (Pretty.prettyBy (Pretty.prettyConfigPlcClassic Pretty.prettyConfigPlcOptions) t)
            , "Env:" Pretty.<+> prettyCekEnv env
            , prettyCekContext ctx
            ]
       )
prettyCekState (Returning ctx val) =
  "Returning:\n"
    <> ( Pretty.indent 2 $
          Pretty.vcat
            [ "Value:" Pretty.<+> Pretty.prettyBy (Pretty.prettyConfigPlcClassic Pretty.prettyConfigPlcOptions) val
            , prettyCekContext ctx
            ]
       )
prettyCekState (Terminating t) =
  "Terminating: " Pretty.<+> Pretty.pretty t

cekTrans ::
  forall uni fun ann s.
  (ThrowableBuiltins uni fun, GivenCekReqs uni fun ann s) =>
  CekTrans uni fun ann s
cekTrans = \case
  Starting term -> pure $ Computing NoFrame RAL.empty term
  Computing ctx env term -> computeCek ctx env term
  Returning ctx val -> returnCek ctx val
  self@Terminating {} -> pure self -- FINAL STATE, idempotent

a =
  case funnerer of
    (Left err, _, _) -> error $ show err
    (Right res, _, _) -> forM_ res (print . prettyCekState)

{- $>
import Prelude
import Plutarch.Fun
import Plutarch
import Plutarch.Prelude
import Plutarch.Pretty
import Control.Monad
<$
-}

-- $> prettyTermAndCost mempty $ foo

-- $> a

f :: Term s (PInteger :--> PInteger)
f = plam $ const 10

fa :: Term s (PInteger :--> PInteger)
fa = plam (\_ x -> f # x) # pconstant @PInteger 1
