{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plutarch (PTInteger, PTByteString, PTString, PTUnit, PTBool, PTList, PTPair, PTData, PTDelayed, PTOpaque, PTSum, PTNew, POrd, (£<=), PEq, (£==), pPair, pinl, pfst, psnd, new, unnew, fromText, PType(..), force, delay, perror, app, (£), (£$), plet, (:-->), (-->), coerce, lam, Term, TermInterp, RawTerm, compile) where
  
import qualified UntypedPlutusCore as UPLC
import qualified PlutusCore as PLC
import PlutusCore.DeBruijn (DeBruijn(DeBruijn), Index(Index))
import Plutus.V1.Ledger.Scripts (Script(Script))
import Data.Kind (Type)
import Numeric.Natural (Natural)
import Data.Text (Text)
import GHC.TypeLits (Symbol)

type RawTerm = UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()

data PType where
  PTInteger :: PType
  PTByteString :: PType
  PTString :: PType
  PTUnit :: PType
  PTBool :: PType
  PTList :: PType -> PType
  PTPair :: PType -> PType -> PType
  PTFun :: PType -> PType -> PType
  PTData :: PType
  PTDelayed :: PType -> PType
  PTOpaque :: PType -- We don't support any kind of abstraction over types after all
  -- Each element represents a constructor and its parameters.
  PTSum :: [[PType]] -> PType
  PTNew :: Symbol -> PType -> PType

type PTInteger = 'PTInteger
type PTByteString = 'PTByteString
type PTString = 'PTString
type PTUnit = 'PTUnit
type PTBool = 'PTBool
type PTList = 'PTList
type PTPair = 'PTPair
type PTData = 'PTData
type PTDelayed = 'PTDelayed
type PTOpaque = 'PTOpaque
type PTSum = 'PTSum
type PTNew = 'PTNew

type (:-->) :: PType -> PType -> PType
type (:-->) = 'PTFun
infixr 0 :-->

(-->) :: PType -> PType -> PType
(-->) = PTFun
infixr 0 -->

class PEq exp (t :: PType) where
  (£==) :: exp t -> exp t -> exp 'PTBool

class POrd exp (t :: PType) where
  (£<=) :: exp t -> exp t -> exp 'PTBool

infix 4 £==

-- We use a type class rather than a GADT, because this
-- makes it impossible to have exotic terms in cLam, that e.g. pattern
-- match on the argument.
--
-- We don't export the members such that we can change them.
class
  ( PEq exp 'PTInteger
  , POrd exp 'PTInteger
  , Num (exp 'PTInteger)
  ) => TermInterp (exp :: PType -> Type) where
  cLam :: (exp a -> exp b) -> exp ('PTFun a b)
  cApp :: exp ('PTFun a b) -> exp a -> exp b
  cForce :: exp a -> exp ('PTDelayed a)
  cDelay :: exp ('PTDelayed a) -> exp a
  cString :: Text -> exp 'PTString
  cError :: exp a
  cCoerce :: exp a -> exp b
  cNew :: exp a -> exp ('PTNew s a)
  cUnnew :: exp ('PTNew s a) -> exp a
  cBuiltin :: UPLC.DefaultFun -> exp a

type Term a = forall exp. TermInterp exp => exp a

-- Source: Unembedding Domain-Specific Languages by Robert Atkey, Sam Lindley, Jeremy Yallop
-- Thanks!
newtype DB a = DB { unDB :: Natural -> RawTerm }

instance PEq DB 'PTInteger where
  x £== y = cApp (cApp (cBuiltin PLC.EqualsInteger) x) y

instance POrd DB 'PTInteger where
  x £<= y = cApp (cApp (cBuiltin PLC.LessThanEqualsInteger) x) y

instance Num (DB 'PTInteger) where
  x + y = cApp (cApp (cBuiltin PLC.AddInteger) x) y
  x - y = cApp (cApp (cBuiltin PLC.SubtractInteger) x) y
  x * y = cApp (cApp (cBuiltin PLC.MultiplyInteger) x) y
  abs x = cApp (cApp (cApp (cBuiltin PLC.IfThenElse) (x £<= (-1))) (negate x)) x -- FIXME use let
  negate x = x - 0
  signum = undefined
  fromInteger n = DB $ \_ -> UPLC.Constant () $ PLC.Some (PLC.ValueOf PLC.DefaultUniInteger n)

instance TermInterp DB where
  cLam f = DB $ \i ->
    let v = DB $ \j -> UPLC.Var () (DeBruijn . Index $ j - (i + 1)) in
    UPLC.LamAbs () undefined $ unDB (f v) (i + 1)
  cApp x y = DB $ \i -> UPLC.Apply () (unDB x i) (unDB y i)
  cForce x = DB $ \i -> UPLC.Force () (unDB x i)
  cDelay x = DB $ \i -> UPLC.Delay () (unDB x i)
  cError = DB $ \_ -> UPLC.Error ()
  cCoerce (DB x) = DB x
  cString s = DB $ \_ -> UPLC.Constant () $ PLC.Some (PLC.ValueOf PLC.DefaultUniString s)
  cNew (DB x) = DB x
  cUnnew (DB x) = DB x
  cBuiltin f = DB $ \_ -> UPLC.Builtin () f

compile :: Term a -> Script
compile t = Script $ UPLC.Program () (PLC.defaultVersion ()) (unDB t 0)

-- utility

lam :: TermInterp exp => (exp a -> exp b) -> exp (a :--> b)
lam = cLam

plet :: TermInterp exp => exp a -> (exp a -> exp b) -> exp b
plet v f = cApp (cLam f) v

-- Should err if `v` is used more than once
pinl :: TermInterp exp => exp a -> (exp a -> exp b) -> exp b
pinl v f = f v

app :: TermInterp exp => exp (a :--> b) -> exp a -> exp b
app = cApp

perror :: TermInterp exp => exp a
perror = cError

(£) :: TermInterp exp => exp (a :--> b) -> exp a -> exp b
(£) = cApp
infixl 8 £

(£$) :: TermInterp exp => exp (a :--> b) -> exp a -> exp b
(£$) = cApp
infixr 0 £$

force :: TermInterp exp => exp a -> exp ('PTDelayed a)
force = cForce

delay :: TermInterp exp => exp ('PTDelayed a) -> exp a
delay = cDelay

coerce :: TermInterp exp => exp a -> exp b
coerce = cCoerce

new :: TermInterp exp => exp a -> exp ('PTNew s a)
new = cNew
unnew :: TermInterp exp => exp ('PTNew s a) -> exp a
unnew = cUnnew

pfst :: TermInterp exp => exp ('PTPair a b) -> exp a
pfst = undefined

psnd :: TermInterp exp => exp ('PTPair a b) -> exp b
psnd = undefined

pPair :: TermInterp exp => exp a -> exp b -> exp ('PTPair a b)
pPair _ _ = undefined

fromText :: TermInterp exp => Text -> exp 'PTString
fromText = cString

_example1 :: Term ('PTInteger :--> 'PTInteger :--> 'PTInteger)
_example1 = lam $ \x -> lam $ \y -> x + y + 200

_example2 :: Term ('PTInteger :--> 'PTInteger :--> 'PTInteger)
_example2 =
  lam $ \x -> lam $ \y ->
  plet (x + y) $ \z ->
  z + z + 200

_example3 :: Term 'PTInteger
_example3 = _example1 £ 100 £ 100

_example4 :: Term ('PTInteger :--> 'PTInteger)
_example4 = lam $ \x -> x + 1

_example5 :: Term 'PTInteger
_example5 = _example4 £$ _example4 £$ _example4 £ 44
