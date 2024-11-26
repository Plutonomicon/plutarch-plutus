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
  PlutusType (PInner),
  DerivePlutusType,
  DPTStrat,
  PlutusTypeScott,
  PlutusTypeNewtype,
  PlutusTypeData,
  PCon,
  PMatch,
  pcon,
  pmatch,
  PForall (PForall),

  -- * Integers and integer utilities
  PInteger,
  PIntegral (pdiv, pmod, pquot, prem),

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
  pif',
  pnot,
  (#&&),
  (#||),
  pcond,

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
  pencodeUtf8,
  pdecodeUtf8,

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

  -- ** Deprecated
  ptrace,
  ptraceShowId,
  ptraceError,
  ptraceIfFalse,
  ptraceIfTrue,

  -- * Cryptographic hashes and signatures
  psha2_256,
  psha3_256,
  pverifySignature,

  -- * Conversion between Plutarch terms and Haskell types
  pconstant,
  pconstantData,
  plift,
  PLiftable (AsHaskell, PlutusRepr),
  PLifted (PLifted),

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
  PTryFrom,
  PSubtype,
  Generic,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import GHC.Records (getField)
import Plutarch.Builtin (
  PAsData,
  PBuiltinList (PCons, PNil),
  PBuiltinPair,
  PData,
  PIsData,
  pconstantData,
  pdata,
  pfromData,
  pfstBuiltin,
  psndBuiltin,
 )
import Plutarch.Builtin.Bool (
  PBool (PFalse, PTrue),
  pcond,
  pif,
  pif',
  pnot,
  (#&&),
  (#||),
 )
import Plutarch.ByteString (
  PByte,
  PByteString,
  PLogicOpSemantics,
  pandBS,
  pbyteToInteger,
  pcomplementBS,
  pconsBS,
  phexByteStr,
  pindexBS,
  pintegerToByte,
  plengthBS,
  ponesBS,
  porBS,
  ppadding,
  preplicateBS,
  psliceBS,
  ptruncation,
  pxorBS,
  pzeroesBS,
 )
import Plutarch.Crypto (psha2_256, psha3_256, pverifySignature)
import Plutarch.DataRepr (
  PDataRecord,
  PDataSum,
  PLabeledType ((:=)),
  PlutusTypeData,
  pdcons,
  pdnil,
  pfield,
  pletFields,
 )
import Plutarch.Either (
  PEither (PLeft, PRight),
 )
import Plutarch.Integer (
  PInteger,
  PIntegral (pdiv, pmod, pquot, prem),
 )
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.Lift (
  PLiftable (AsHaskell, PlutusRepr),
  PLifted (PLifted),
  pconstant,
  plift,
 )
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.Ord (
  POrd (pmax, pmin),
  PPartialOrd ((#<), (#<=), (#>), (#>=)),
 )
import Plutarch.Internal.Other (POpaque (POpaque), pfix, popaque, pto)
import Plutarch.Internal.PLam (pinl, plam)
import Plutarch.Internal.PlutusType (
  DerivePlutusType (DPTStrat),
  PCon,
  PMatch,
  PlutusType (PInner),
  pcon,
  pmatch,
 )
import Plutarch.Internal.Quantification (PForall (PForall))
import Plutarch.Internal.ScottEncoding (PlutusTypeScott)
import Plutarch.Internal.Term (
  ClosedTerm,
  PDelayed,
  S,
  Term,
  papp,
  pdelay,
  perror,
  pforce,
  phoistAcyclic,
  plet,
  pthrow,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.List (
  PElemConstraint,
  PIsListLike,
  PList (PSCons, PSNil),
  PListLike,
  pall,
  pany,
  pconcat,
  pcons,
  pdrop,
  pelem,
  pelemAt,
  pelimList,
  pfilter,
  pfind,
  pfoldl,
  pfoldr,
  pfoldrLazy,
  phead,
  plength,
  plistEquals,
  pmap,
  pnil,
  pnull,
  precList,
  psingleton,
  ptail,
  ptryIndex,
  pzip,
  pzipWith,
  pzipWith',
  (#!!),
 )
import Plutarch.Maybe (
  PMaybe (PJust, PNothing),
 )
import Plutarch.Pair (
  PPair (PPair),
 )
import Plutarch.Rational (
  PRational (PRational),
  pdenominator,
  pnumerator,
  pround,
 )
import Plutarch.Show (
  PShow,
  pshow,
 )
import Plutarch.String (
  PString,
  pdecodeUtf8,
  pencodeUtf8,
 )
import Plutarch.TermCont (
  TermCont (TermCont),
  pguardC,
  pguardC',
  pletC,
  pletFieldsC,
  pmatchC,
  ptraceC,
  ptryFromC,
  runTermCont,
  tcont,
  unTermCont,
 )
import Plutarch.Trace (
  ptrace,
  ptraceDebug,
  ptraceDebugError,
  ptraceDebugIfFalse,
  ptraceDebugIfTrue,
  ptraceDebugShowId,
  ptraceError,
  ptraceIfFalse,
  ptraceIfTrue,
  ptraceInfo,
  ptraceInfoError,
  ptraceInfoIfFalse,
  ptraceInfoIfTrue,
  ptraceInfoShowId,
  ptraceShowId,
 )
import Plutarch.TryFrom (
  PSubtype,
  PTryFrom,
  ptryFrom,
  pupcast,
 )
import Plutarch.Unit (PUnit (PUnit))
import Prelude ()
