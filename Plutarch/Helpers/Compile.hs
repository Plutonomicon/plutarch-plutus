module Plutarch.Helpers.Compile (
  compileTerm,
  termToUPLC,
) where

import Control.Monad.Except (runExceptT)
import Control.Monad.RWS.CPS (runRWS)
import Data.Kind (Type)
import Plutarch.Backend.ANF (fromHashedAST, fullPipeline)
import Plutarch.Backend.AST (fromRawTerm)
import Plutarch.Backend.Compile (toUPLCTerm)
import Plutarch.Backend.RawTerm (RawTerm)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term (Term), TermEnv (TermEnv), TermError)
import Plutarch.Backend.UPLC (UPLCTerm)

-- | @since wip
compileTerm ::
  forall (a :: S -> Type).
  (forall (s :: S). Term s a) ->
  Either TermError (RawTerm ())
compileTerm (Term comp) = (\(x, _, _) -> fmap snd x) . runRWS (runExceptT comp) TermEnv $ 0

-- | @since wip
termToUPLC ::
  forall (a :: S -> Type).
  (forall (s :: S). Term s a) ->
  Either TermError UPLCTerm
termToUPLC t = toUPLCTerm . fullPipeline . fromHashedAST . fromRawTerm <$> compileTerm t
