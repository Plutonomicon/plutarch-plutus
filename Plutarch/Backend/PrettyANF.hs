module Plutarch.Backend.PrettyANF where

import Plutarch.Backend.ANF
import Plutarch.Backend.AST (Hash (Hash), Multiplicity (MultiplicityMany, MultiplicityOne), fromRawTerm)
import Plutarch.Backend.UPLC (UPLCTerm (UPLCTerm))

import PlutusCore (Some (Some), someValue)
import Prettyprinter

import Data.Vector qualified as Vector
import Data.Vector.NonEmpty qualified as NEV

import Data.Text (Text)
import Data.Text qualified as T

-- for testing / delete later

import Control.Monad.Except (runExceptT)
import Control.Monad.RWS.CPS (runRWS)
import Data.Kind (Type)
import Plutarch.Backend.RawTerm (RawTerm)
import Plutarch.Backend.Term (
  PDelayed,
  S,
  Term,
  TermEnv (TermEnv),
  TermError,
  asRawTerm,
  papp,
  pdelay,
  pforce,
  plam',
  punsafeConstant,
  (:-->),
 )
import Plutarch.Backend.VarMap (VarMap)
import Plutarch.Primitive.Bool (PBool, pnot, por)

compileTerm ::
  forall (a :: S -> Type) (s :: S).
  Term s a -> Either TermError (VarMap, RawTerm ())
compileTerm t = case runRWS (runExceptT (asRawTerm t)) TermEnv 0 of
  (res, _, _) -> res

testPrettyANF :: forall (s :: S) a ann. Term s a -> Doc ann
testPrettyANF t = case compileTerm t of
  Left err -> error (show err)
  Right (_vm, rt) ->
    let anf = fromHashedAST $ fromRawTerm rt
     in prettyANF anf

(#) :: forall (s :: S) a b. Term s (a :--> b) -> Term s a -> Term s b
(#) = papp
infixl 8 #

troo :: Term s PBool
troo = punsafeConstant $ someValue True

-- Case 1: \x -> (\y -> y) ((\z -> z) x)
case1 :: forall (a :: S -> Type) (s :: S). Term s (a :--> a)
case1 = plam' $ \x -> papp (plam' id) (papp (plam' id) x)

-- Case 2: \x -> force (delay x)
case2 :: forall (a :: S -> Type) (s :: S). Term s (a :--> a)
case2 = plam' $ \x -> pforce (pdelay x)

-- Case 3: \x y -> por (pnot x) y
case3 :: forall (s :: S). Term s (PBool :--> PBool :--> PBool)
case3 = plam' $ \x -> plam' $ \y -> por (pnot x) y

case4 :: forall (s :: S). Term s (PBool :--> PDelayed PBool)
case4 = plam' $ \x -> pdelay x

case5 :: forall {s :: S}. Term s PBool
case5 = pforce $ case1 # (case4 # (case1 # (case3 # troo # troo)))

prettyANF :: forall ann1 ann2. ANF ann1 -> Doc ann2
prettyANF (ANF _ binds) = vcat . NEV.toList $ NEV.imap (\(Id -> i) b -> mkBind i b) binds
  where
    mkBind :: Id -> ANFBind ann1 -> Doc ann2
    mkBind i b = align . group $ prettyId i <+> ":=" <+> align (group $ prettyBind b)

prettyBind :: forall ann1 ann2. ANFBind ann1 -> Doc ann2
prettyBind = \case
  ANFLeaf l -> prettyLeaf l
  ANFForce _ ref -> "!" <> prettyRef ref
  ANFDelay _ ref -> angles (prettyRef ref)
  ANFLam _ args body -> "\\" <> mkArgs args <+> "->" <+> prettyRef body
  ANFFix _ mult body -> "FIX" <> brackets (prettyMultiplicity mult) <+> prettyRef body
  ANFApply _ fnRef args -> hsep . punctuate " #" . fmap prettyRef $ (fnRef : NEV.toList args)
  ANFConstr _ cix args -> "CTOR" <+> viaShow cix <+> list (prettyRef <$> Vector.toList args)
  ANFCase _ scrut handlers -> "case" <+> prettyRef scrut <+> list (prettyRef <$> NEV.toList handlers)
  where
    mkArgs :: NEV.NonEmptyVector (Maybe Multiplicity) -> Doc ann
    mkArgs (NEV.toList -> xs) =
      hsep
        . fmap (\case Nothing -> "_"; Just m -> prettyMultiplicity m)
        $ xs

prettyLeaf :: forall ann1 ann2. Leaf ann1 -> Doc ann2
prettyLeaf = \case
  LConstant _ (Some plcVal) ->
    -- the default instance is ugly, TODO prettify it later
    pretty plcVal
  LBuiltin _ fun -> viaShow fun
  LCompiled _ (UPLCTerm uplc) -> pretty uplc
  LError _ -> "ERROR"

prettyRef :: forall ann. Ref -> Doc ann
prettyRef = \case
  AVar h -> prettyHash h
  AnId i -> prettyId i

prettyMultiplicity :: forall ann. Multiplicity -> Doc ann
prettyMultiplicity = \case
  MultiplicityOne h -> prettyHash h
  MultiplicityMany h -> prettyHash h

-- Hashing will give huge ints which are hard to read
-- so we turn them into something readable
prettyHash :: forall ann. Hash -> Doc ann
prettyHash (Hash h) = pretty . compactReadableVar . fromIntegral $ h

-- IDs should usually be small enough to easily read
prettyId :: Id -> Doc ann
prettyId (Id i) = "Id" <> brackets (viaShow i)

compactReadableVar :: Integer -> Text
compactReadableVar n
  | n < 0 = compactReadableVar (abs n) <> "N"
  | dn == 0 = T.singleton (lowers Vector.! fromIntegral mn)
  | otherwise = T.singleton (lowers Vector.! fromIntegral mn) <> go dn
  where
    (dn, mn) = n `divMod` 26

    go :: Integer -> Text
    go x = case x `divMod` 61 of
      (dx, mx) ->
        if dx == 0
          then T.singleton (allChars Vector.! fromIntegral mx)
          else
            T.singleton (allChars Vector.! fromIntegral mx)
              <> go dx

    lowers :: Vector.Vector Char
    lowers = Vector.fromList ['a' .. 'z']

    allChars :: Vector.Vector Char
    allChars = lowers <> Vector.fromList (['A' .. 'M'] <> ['O' .. 'Z'] <> ['0' .. '9'])
