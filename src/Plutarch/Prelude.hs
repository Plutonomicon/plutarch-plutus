module Plutarch.Prelude (
  -- * eDSL types and functions.
  (:-->),
  PDelayed,
  Term,
  ClosedTerm,
  plam,
  papp,
  pdelay,
  pforce,
  phoistAcyclic,
  perror,
  (#$),
  (#),
  plet,
  pinl,
  pto,
  pfix,
  pthrow,
  Type,
  S,
  PType,
  PlutusType (..),
  DerivePlutusType,
  DPTStrat,
  PlutusTypeScott,
  PlutusTypeNewtype,
  PlutusTypeData,
  PUnsafeLiftDecl (..),
  PConstantDecl (..),
  pcon,
  pmatch,
  PForall (PForall),

  -- * Numerical type classes
  PNum (..),
  PIntegral (..),

  -- * Integral types
  PInteger,
  PPositive,
  ppositive,
  ptryPositive,
  Positive,
  mkPositive,

  -- * Rational numbers and utilities
  PRational (PRational),
  pnumerator,
  pdenominator,
  pround,

  -- * Booleans and boolean functions
  PBool (..),
  PEq ((#==)),
  PPartialOrd (..),
  POrd (..),
  pif,
  pnot,
  (#&&),
  (#||),
  pand',
  por',

  -- * Bytestrings and bytestring utilities

  -- ** Types
  PByteString,
  PByte,
  PLogicOpSemantics,

  -- ** Functions

  -- *** Construction
  ppadding,
  ptruncation,
  pzeroesBS,
  ponesBS,
  preplicateBS,

  -- *** Byte-oriented
  pandBS,
  porBS,
  pxorBS,
  pcomplementBS,
  pconsBS,
  psliceBS,
  plengthBS,
  pindexBS,

  -- *** Conversion
  pbyteToInteger,
  pintegerToByte,

  -- *** Other
  phexByteStr,

  -- * String and string utilities
  PString,
  pbuiltinEncodeUtf8,
  pbuiltinDecodeUtf8,

  -- * Unit type and utilities
  PUnit (..),

  -- * Common list typeclass and utilities
  PListLike (PElemConstraint, pelimList, pcons, pnil, phead, ptail, pnull),
  PIsListLike,
  plistEquals,
  pelem,
  pelemAt,
  plength,
  ptryIndex,
  pdrop,
  psingleton,
  pconcat,
  pzipWith,
  pzipWith',
  pzip,
  pmap,
  pfilter,
  pfind,
  precList,
  pfoldr,
  pfoldrLazy,
  pfoldl,
  pall,
  pany,
  (#!!),

  -- * Scott encoded list type
  PList (..),

  -- * Scott encoded maybe type and utilities
  PMaybe (..),

  -- * Scott encoded either type and utilities
  PEither (..),

  -- * Scott encoded pair type and utilities
  PPair (..),

  -- * Opaque type
  POpaque (POpaque),
  popaque,

  -- * Builtin types and utilities
  PData,
  pfstBuiltin,
  psndBuiltin,
  PBuiltinPair,
  PBuiltinList (..),
  PIsData,
  pfromData,
  pdata,
  PAsData,

  -- * DataRepr and related functions
  PDataFields,
  PDataRecord,
  PDataSum,
  PLabeledType ((:=)),
  pdcons,
  pdnil,
  pfield,
  getField,
  pletFields,

  -- * Tracing
  PShow,
  pshow,
  ptraceInfo,
  ptraceDebug,
  ptraceInfoShowId,
  ptraceInfoIfFalse,
  ptraceInfoIfTrue,
  ptraceInfoError,
  ptraceDebugShowId,
  ptraceDebugIfFalse,
  ptraceDebugIfTrue,
  ptraceDebugError,

  -- * Cryptographic hashes and signatures
  psha2_256,
  psha3_256,

  -- * Conversion between Plutarch terms and Haskell types
  pconstant,
  pconstantData,
  plift,
  PConstant,
  PLift,
  PConstantData,
  PLiftData,

  -- * Continuation monad
  TermCont (TermCont, runTermCont),
  unTermCont,
  tcont,
  pletC,
  pmatchC,
  pletFieldsC,
  ptraceC,
  pguardC,
  pguardC',
  ptryFromC,
  pupcast,
  ptryFrom,
  PTryFrom (..),
  PSubtype,
  Generic,

  -- * Derivation helpers
  DerivePConstantViaData (..),
  DerivePConstantViaNewtype (..),
  DerivePConstantViaBuiltin (..),
  PDataNewtype (..),

  -- * Compilation
  Config (..),
  LogLevel (..),
  TracingMode (..),
  compile,
  compileOptimized,

  -- * Enumerable and countable
  PCountable (..),
  PEnumerable (..),

  -- * Unsafe operations
  punsafeCoerce,
  punsafeBuiltin,
  punsafeDowncast,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import GHC.Records (getField)
import Plutarch.Builtin
import Plutarch.Builtin.Bool (PBool (..))
import Plutarch.Builtin.ByteString (PByteString)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Builtin.Lift (
  DerivePConstantViaBuiltin (..),
  DerivePConstantViaNewtype (..),
  PConstant,
  PConstantDecl (..),
  PLift,
  PUnsafeLiftDecl (..),
  pconstant,
  plift,
 )
import Plutarch.Builtin.Opaque (POpaque (..))
import Plutarch.Builtin.String (
  PString,
  pbuiltinDecodeUtf8,
  pbuiltinEncodeUtf8,
 )
import Plutarch.Builtin.Unit (PUnit (..))
import Plutarch.ByteString
import Plutarch.Crypto
import Plutarch.DataRepr
import Plutarch.Either
import Plutarch.Enum
import Plutarch.Internal.Builtin
import Plutarch.Internal.Eq
import Plutarch.Internal.Newtype
import Plutarch.Internal.Numeric
import Plutarch.Internal.Ord
import Plutarch.Internal.PlutusType
import Plutarch.Internal.Quantification
import Plutarch.Internal.ScottEncoding
import Plutarch.Internal.Show (PShow (..), pshow)
import Plutarch.Internal.Term
import Plutarch.List
import Plutarch.Maybe
import Plutarch.Pair
import Plutarch.Positive
import Plutarch.Rational
import Plutarch.TermCont
import Plutarch.Trace
import Plutarch.TryFrom
import Prelude ()

{- | Forget the type of a term.

@since WIP
-}
popaque ::
  forall (a :: S -> Type) (s :: S).
  Term s a ->
  Term s POpaque
popaque = punsafeCoerce
