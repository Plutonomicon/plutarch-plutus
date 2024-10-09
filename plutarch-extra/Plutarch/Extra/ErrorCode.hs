module Plutarch.Extra.ErrorCode (errorTagReplacer, codifyStrings) where

import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple (swap)
import Plutarch.Script (Script (Script))
import PlutusCore (DefaultUni (..))
import Universe (Some (Some), ValueOf (ValueOf))
import UntypedPlutusCore qualified as UPLC

-- | Replace any string that is tagged with "ERR:" prefix will get replaced by a unique integer id
errorTagReplacer :: Int -> Text -> Maybe Text
errorTagReplacer n s =
  if T.isPrefixOf "ERR:" s
    then Just $ "CODE:" <> T.pack (show n)
    else Nothing

-- | Replace strings satisfying the predicate with provided replacement
codifyStrings :: (Int -> Text -> Maybe Text) -> Script -> (Script, [(Text, Text)])
codifyStrings codify (Script (UPLC.Program ann version terms)) =
  let
    immap :: forall a b. (Int -> a -> Maybe b) -> [a] -> [(a, b)]
    immap f xs = go 0 xs
      where
        go _ [] = []
        go n (x : xs) =
          case f n x of
            Just x' -> (x, x') : go (n + 1) xs
            Nothing -> go (n + 1) xs

    messagesIds = immap codify $ nub $ collectErrorMessages terms
   in
    (Script (UPLC.Program ann version (replaceMessages messagesIds terms)), swap <$> messagesIds)
  where
    -- \| Scripts all Strings form the script
    collectErrorMessages :: UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun () -> [Text]
    collectErrorMessages (UPLC.Var _ _) = []
    collectErrorMessages (UPLC.LamAbs _ _ uterm) = collectErrorMessages uterm
    collectErrorMessages (UPLC.Apply _ uterm1 uterm2) =
      collectErrorMessages uterm1 <> collectErrorMessages uterm2
    collectErrorMessages (UPLC.Force _ uterm) = collectErrorMessages uterm
    collectErrorMessages (UPLC.Delay _ uterm) = collectErrorMessages uterm
    collectErrorMessages (UPLC.Constant _ (Some (ValueOf DefaultUniString str))) = [str]
    collectErrorMessages (UPLC.Constant _ _) = []
    collectErrorMessages (UPLC.Builtin _ _) = []
    collectErrorMessages (UPLC.Error _) = []
    collectErrorMessages (UPLC.Constr _ _ uterms) = foldMap collectErrorMessages uterms
    collectErrorMessages (UPLC.Case _ uterm uterms) =
      collectErrorMessages uterm <> foldMap collectErrorMessages uterms

    replaceMessages ::
      [(Text, Text)] ->
      UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun () ->
      UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
    replaceMessages _l t@(UPLC.Var _ _) = t
    replaceMessages l (UPLC.LamAbs ann name uterm) = UPLC.LamAbs ann name (replaceMessages l uterm)
    replaceMessages l (UPLC.Apply ann uterm1 uterm2) =
      UPLC.Apply ann (replaceMessages l uterm1) (replaceMessages l uterm2)
    replaceMessages l (UPLC.Force ann uterm) = UPLC.Force ann (replaceMessages l uterm)
    replaceMessages l (UPLC.Delay ann uterm) = UPLC.Delay ann (replaceMessages l uterm)
    replaceMessages l (UPLC.Constant ann (Some (ValueOf DefaultUniString str))) =
      UPLC.Constant ann (Some (ValueOf DefaultUniString (fromMaybe str $ lookup str l)))
    replaceMessages _l t@(UPLC.Constant _ _) = t
    replaceMessages _l t@(UPLC.Builtin _ _) = t
    replaceMessages _l t@(UPLC.Error _) = t
    replaceMessages l (UPLC.Constr ann idx uterms) =
      UPLC.Constr ann idx (replaceMessages l <$> uterms)
    replaceMessages l (UPLC.Case ann uterm uterms) =
      UPLC.Case ann (replaceMessages l uterm) (replaceMessages l <$> uterms)
