-- hehe
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-all #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}

module Plutarch.Fun where

import Control.Monad
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce
import Data.Functor.Identity
import Data.List (intercalate, nub)
import Data.Map qualified as Map
import Data.RandomAccessList.Class qualified as RAL
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Debug.Trace
import Plutarch
import Plutarch.Internal
import Plutarch.Internal.PLam
import Plutarch.Prelude
import Plutarch.Script (NamedScript (unNamedScript), Script (unScript))
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCekParametersForTesting)
import PlutusCore.Pretty qualified as Pretty
import PlutusCore.Quote
import Prettyprinter qualified as Pretty
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek
import UntypedPlutusCore.Evaluation.Machine.Cek.CekMachineCosts
import UntypedPlutusCore.Evaluation.Machine.SteppableCek.Internal

import PlutusLedgerApi.V3

import GHC.Stack

{-# ANN module "HLint: ignore" #-}

cs :: HasCallStack => String -> String
cs s = s <> prettyCallStack callStack

g :: HasCallStack => String
g = cs "This string is being passed to"

f :: HasCallStack => String
f = g

myAdd :: HasCallStack => Term s (PInteger :--> PInteger :--> PInteger)
myAdd = plam $ \x y -> x + x + y

mySub :: HasCallStack => Term s (PInteger :--> PInteger :--> PInteger)
mySub = plam $ \x y -> x - y - y

foo :: HasCallStack => Term s PInteger
foo = myAdd # (myAdd # 10 # 20) # (myAdd # 10 # 20)

-- (plamNamed "outer" $ \y -> plamNamed "foo" (\_x _z -> _x) # (pconstant @PInteger 0) #$ (myAdd # 10 # y)) # 2

rawScript :: UPLC.Term UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
rawScript =
  UPLC.LamAbs () (UPLC.NamedDeBruijn "hello" 99) (UPLC.LamAbs () (UPLC.NamedDeBruijn "world" 0) (UPLC.Var () (UPLC.NamedDeBruijn "foo" 1)))

fun :: NamedScript
fun =
  case compileNamed mempty foo of
    Left _ -> error "no"
    Right x -> x

funner :: CekState UPLC.DefaultUni UPLC.DefaultFun ()
funner =
  -- Starting (UPLC.termMapNames UPLC.fakeNameDeBruijn $ UPLC._progTerm . unScript $ fun)
  Starting rawScript

raToList :: (RAL.RandomAccessList e, a ~ RAL.Element e) => e -> [a]
raToList x =
  case RAL.uncons x of
    Nothing -> []
    Just (h, r) -> h : raToList r

unboundVariables :: UPLC.Term UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun () -> [UPLC.NamedDeBruijn]
unboundVariables t = nub $ go 0 t
  where
    go :: Integer -> UPLC.Term UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun () -> ([UPLC.NamedDeBruijn])
    go idx (UPLC.Var () ndb@(UPLC.NamedDeBruijn _ n)) = if n > fromIntegral idx then [ndb] else []
    go idx (UPLC.Apply () f t) = go idx f <> go idx t
    go idx (UPLC.LamAbs () n body) = go (idx + 1) body
    go idx (UPLC.Force () t) = go idx t
    go idx (UPLC.Delay () t) = go idx t
    go idx (UPLC.Constant _ _) = []
    go idx (UPLC.Builtin _ _) = []
    go idx (UPLC.Error _) = []
    go idx (UPLC.Constr _ _ ts) = mconcat $ go idx <$> ts
    go idx (UPLC.Case _ t ts) = go idx t <> (mconcat $ Vector.toList (go idx <$> ts))

envWithMatchingVariables :: CekValEnv UPLC.DefaultUni UPLC.DefaultFun () -> UPLC.Term UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun () -> [((CekValue UPLC.DefaultUni UPLC.DefaultFun ()), UPLC.NamedDeBruijn)]
envWithMatchingVariables env term =
  let
    unboundVars = unboundVariables term
    minIdx = foldr1 min $ (\(UPLC.NamedDeBruijn _ idx) -> idx) <$> unboundVars
    f ndb@(UPLC.NamedDeBruijn name idx) =
      case RAL.indexOne env (coerce $ idx - minIdx + 1) of
        Nothing -> error "impossible, cek machine produced open term without matching environment"
        Just x -> (x, ndb)
   in
    if null unboundVars then [] else f <$> unboundVars

envWithMatchingVariablesPretty :: CekValEnv UPLC.DefaultUni UPLC.DefaultFun () -> UPLC.Term UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun () -> Pretty.Doc ()
envWithMatchingVariablesPretty env term =
  let
    go (t, ndb) =
      prettyNamedDeBruijn ndb
        Pretty.<+> ":"
        Pretty.<+> Pretty.prettyBy (Pretty.prettyConfigPlcClassic Pretty.prettyConfigPlcOptions) t
   in
    Pretty.encloseSep "[" "]" "," (go <$> envWithMatchingVariables env term)

prettyNamedDeBruijn :: UPLC.NamedDeBruijn -> Pretty.Doc ()
prettyNamedDeBruijn (UPLC.NamedDeBruijn name idx) =
  Pretty.pretty name <> "!" <> Pretty.pretty idx

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
            [ "Term:" Pretty.<+> (Pretty.align (Pretty.prettyBy (Pretty.prettyConfigPlcClassic Pretty.prettyConfigPlcOptions) t))
            , "Env:" Pretty.<+> envWithMatchingVariablesPretty env t
            , "EnvRaw:" Pretty.<+> prettyCekEnv env
            , -- prettyCekEnv env
              prettyCekContext ctx
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

generateExecutionSteps ::
  NamedScript ->
  ( Either
      (CekEvaluationException UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun)
      [((CekState UPLC.DefaultUni UPLC.DefaultFun ()), ExBudget)]
  , Cek.RestrictingSt
  , [Text.Text]
  )
generateExecutionSteps term = runCekM (defaultCekParametersForTesting @()) Cek.restrictingEnormous Cek.logEmitter do
  (trans, budgetInfo) <-
    mkCekTrans
      (defaultCekParametersForTesting @())
      Cek.restrictingEnormous
      Cek.logEmitter
      nilSlippage

  let
    (ExBudgetInfo spender _final cumulative) = budgetInfo

    go (Terminating _) = pure []
    go x = do
      nextCost <- CekM cumulative
      nextState <- trans x

      ((x, nextCost) :) <$> go nextState

  (unCekBudgetSpender spender) BStartup (runIdentity $ cekStartupCost ?cekCosts)

  steps <- go (Starting (UPLC._progTerm . unNamedScript $ term))
  pure steps

newtype TermText
  = TermText (UPLC.Term UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun ())
  deriving stock (Generic, Show)

instance Aeson.ToJSON TermText where
  toJSON (TermText t) =
    -- Pretty instance given by plutus doesn't respect Pretty.group
    Aeson.toJSON $ unwords $ words $ show $ Pretty.prettyBy (Pretty.prettyConfigPlcClassic Pretty.prettyConfigPlcOptions) t

data DebuggerBudget
  = DebuggerBudget ExBudget
  deriving stock (Generic, Show)

instance Aeson.ToJSON DebuggerBudget where
  toJSON (DebuggerBudget (ExBudget cpu mem)) = do
    Aeson.object
      [ "cpu" Aeson..= Aeson.toJSON cpu
      , "mem" Aeson..= Aeson.toJSON mem
      ]

data DebuggerContext
  = DebuggerContextAwaitArg DebuggerValue
  | DebuggerContextAwaitFunTerm DebuggerEnv TermText
  | DebuggerContextAwaitFunValue DebuggerValue
  | DebuggerContextForce
  | DebuggerContextConstr DebuggerEnv Integer [TermText] [DebuggerValue]
  | DebuggerContextCases DebuggerEnv [TermText]
  | DebuggerContextNoFrame
  deriving stock (Generic, Show)

instance Aeson.ToJSON DebuggerContext

data DebuggerEnv
  = DebuggerEnv ([(DebuggerValue, UPLC.NamedDeBruijn)])
  deriving stock (Generic, Show)

instance Aeson.ToJSON DebuggerEnv where
  toJSON (DebuggerEnv xs) =
    let
      f (val, ndb) =
        (show $ prettyNamedDeBruijn ndb, val)
     in
      Aeson.toJSON $ Map.fromList $ f <$> xs

data DebuggerValue
  = DebuggerValue (CekValue UPLC.DefaultUni UPLC.DefaultFun ())
  deriving stock (Generic, Show)

instance Aeson.ToJSON DebuggerValue where
  toJSON (DebuggerValue t) =
    -- Pretty instance given by plutus doesn't respect Pretty.group
    Aeson.toJSON $ unwords $ words $ show $ Pretty.prettyBy (Pretty.prettyConfigPlcClassic Pretty.prettyConfigPlcOptions) t

data DebuggerState
  = DebuggerStateStarting TermText
  | DebuggerStateComputing [DebuggerContext] DebuggerEnv TermText
  | DebuggerStateReturning [DebuggerContext] DebuggerValue
  | DebuggerStateTerminating TermText
  deriving stock (Generic, Show)

instance Aeson.ToJSON DebuggerState

data DebuggerStep = DebuggerStep {debuggerBudget :: DebuggerBudget, debuggerState :: DebuggerState}
  deriving stock (Generic, Show)

instance Aeson.ToJSON DebuggerStep

cekStateToDebuggerEnv :: [UPLC.Term UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun ()] -> CekValEnv UPLC.DefaultUni UPLC.DefaultFun () -> DebuggerEnv
cekStateToDebuggerEnv ts env =
  let
    unboundVars = foldMap unboundVariables ts
    minIdx = foldr1 min $ (\(UPLC.NamedDeBruijn _ idx) -> idx) <$> unboundVars
    f ndb@(UPLC.NamedDeBruijn name idx) =
      case RAL.indexOne env (coerce $ idx - minIdx + 1) of
        Nothing -> error "impossible, cek machine produced open term without matching environment"
        Just x -> trace (show ndb) $ (DebuggerValue x, ndb)
   in
    -- TODO: Must this only take what's in environment that is also an unbound variable of given term.
    -- Need to include everything
    DebuggerEnv $
      if null unboundVars
        then []
        else f <$> unboundVars

cekStateToDebuggerContext :: Context UPLC.DefaultUni UPLC.DefaultFun () -> [DebuggerContext]
cekStateToDebuggerContext (FrameAwaitArg () val ctx) =
  DebuggerContextAwaitArg (DebuggerValue val) : cekStateToDebuggerContext ctx
cekStateToDebuggerContext (FrameAwaitFunTerm () env t ctx) =
  DebuggerContextAwaitFunTerm (cekStateToDebuggerEnv [t] env) (TermText t) : cekStateToDebuggerContext ctx
cekStateToDebuggerContext (FrameAwaitFunValue () val ctx) =
  DebuggerContextAwaitFunValue (DebuggerValue val) : cekStateToDebuggerContext ctx
cekStateToDebuggerContext (FrameForce () ctx) =
  DebuggerContextForce : cekStateToDebuggerContext ctx
cekStateToDebuggerContext (FrameConstr () env n ts vals ctx) =
  DebuggerContextConstr (cekStateToDebuggerEnv ts env) (toInteger n) (TermText <$> ts) [] : cekStateToDebuggerContext ctx
cekStateToDebuggerContext (FrameCases () env ts ctx) =
  DebuggerContextCases (cekStateToDebuggerEnv (Vector.toList ts) env) (TermText <$> Vector.toList ts) : cekStateToDebuggerContext ctx
cekStateToDebuggerContext NoFrame = []

cekStateToDebuggerState :: CekState UPLC.DefaultUni UPLC.DefaultFun () -> DebuggerState
cekStateToDebuggerState (Starting t) = DebuggerStateStarting $ TermText t
cekStateToDebuggerState (Computing ctx env t) = DebuggerStateComputing (cekStateToDebuggerContext ctx) (cekStateToDebuggerEnv [t] env) (TermText t)
cekStateToDebuggerState (Returning ctx val) = DebuggerStateReturning (cekStateToDebuggerContext ctx) (DebuggerValue val)
cekStateToDebuggerState (Terminating t) = DebuggerStateTerminating $ TermText t

cekStateToDebuggerStep :: (CekState UPLC.DefaultUni UPLC.DefaultFun (), ExBudget) -> DebuggerStep
cekStateToDebuggerStep (state, budget) = DebuggerStep (DebuggerBudget budget) (cekStateToDebuggerState state)

b = TermText (UPLC._progTerm . unNamedScript $ fun)

c =
  case generateExecutionSteps fun of
    (Left err, _, _) -> error $ show err
    (Right res, x, y) -> res

d = Aeson.encode $ Aeson.toJSON $ cekStateToDebuggerStep <$> c

a =
  case generateExecutionSteps fun of
    (Left err, _, _) -> error $ show err
    (Right res, x, y) -> do
      putStrLn $ "steps: " <> show (length res)
      forM_
        res
        ( \(s, b) -> do
            print b
            print $ prettyCekState s
        )

-- $> import Prelude; import Plutarch.Evaluate; import Plutarch.Fun; import Plutarch; import Plutarch.Prelude; import Plutarch.Pretty; import Control.Monad; import Data.ByteString.Lazy qualified as LBS

-- $> :set -Wno-incomplete-uni-patterns

-- $> LBS.putStrLn d

-- $> b

-- $> let Right (_, x, _) = evalTerm mempty $ foo in x

-- $> a
