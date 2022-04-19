{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "2.4";
      identifier = { name = "plutarch"; version = "1.1.0"; };
      license = "MIT";
      copyright = "";
      maintainer = "";
      author = "Las Safin <me@las.rs>";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."rank2classes" or (errorHandler.buildDepError "rank2classes"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        modules = [
          "Plutarch"
          "Plutarch/Api/Internal/Scripts"
          "Plutarch/Api/V1"
          "Plutarch/Api/V1/Address"
          "Plutarch/Api/V1/AssocMap"
          "Plutarch/Api/V1/Contexts"
          "Plutarch/Api/V1/Crypto"
          "Plutarch/Api/V1/DCert"
          "Plutarch/Api/V1/Interval"
          "Plutarch/Api/V1/Maybe"
          "Plutarch/Api/V1/Scripts"
          "Plutarch/Api/V1/Time"
          "Plutarch/Api/V1/Tuple"
          "Plutarch/Api/V1/Tx"
          "Plutarch/Api/V1/Value"
          "Plutarch/Bool"
          "Plutarch/Builtin"
          "Plutarch/ByteString"
          "Plutarch/Crypto"
          "Plutarch/DataRepr"
          "Plutarch/DataRepr/Internal"
          "Plutarch/DataRepr/Internal/Field"
          "Plutarch/DataRepr/Internal/FromData"
          "Plutarch/DataRepr/Internal/HList"
          "Plutarch/DataRepr/Internal/HList/Utils"
          "Plutarch/Either"
          "Plutarch/Evaluate"
          "Plutarch/FFI"
          "Plutarch/Integer"
          "Plutarch/Internal"
          "Plutarch/Internal/Generic"
          "Plutarch/Internal/Other"
          "Plutarch/Internal/PLam"
          "Plutarch/Internal/PlutusType"
          "Plutarch/Internal/TypeFamily"
          "Plutarch/Lift"
          "Plutarch/List"
          "Plutarch/Maybe"
          "Plutarch/Monadic"
          "Plutarch/Pair"
          "Plutarch/Prelude"
          "Plutarch/Rational"
          "Plutarch/Rec"
          "Plutarch/Rec/TH"
          "Plutarch/Reducible"
          "Plutarch/Show"
          "Plutarch/String"
          "Plutarch/TermCont"
          "Plutarch/Trace"
          "Plutarch/TryFrom"
          "Plutarch/Unit"
          "Plutarch/Unsafe"
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.; }