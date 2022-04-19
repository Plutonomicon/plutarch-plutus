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
      identifier = { name = "plutarch-test"; version = "1.1.0"; };
      license = "NONE";
      copyright = "";
      maintainer = "";
      author = "";
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
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
          (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
          (hsPkgs."hspec-discover" or (errorHandler.buildDepError "hspec-discover"))
          (hsPkgs."hspec-golden" or (errorHandler.buildDepError "hspec-golden"))
          (hsPkgs."hspec-hedgehog" or (errorHandler.buildDepError "hspec-hedgehog"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."plutarch" or (errorHandler.buildDepError "plutarch"))
          (hsPkgs."plutarch-extra" or (errorHandler.buildDepError "plutarch-extra"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        modules = [
          "Plutarch/Test/Benchmark"
          "Plutarch/Test/Golden"
          "Plutarch/Test/ListSyntax"
          "Plutarch/Test/Property/Extra"
          "Plutarch/Test/Property/Gen"
          "Plutarch/Test/Property/HaskEquiv"
          "Plutarch/Test/Property/Marshal"
          "Plutarch/Test/Run"
          "Plutarch/Test"
          "Plutarch/Test/Property"
          ];
        hsSourceDirs = [ "common" ];
        };
      exes = {
        "plutarch-test" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
            (hsPkgs."hspec-discover" or (errorHandler.buildDepError "hspec-discover"))
            (hsPkgs."hspec-golden" or (errorHandler.buildDepError "hspec-golden"))
            (hsPkgs."hspec-hedgehog" or (errorHandler.buildDepError "hspec-hedgehog"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutarch" or (errorHandler.buildDepError "plutarch"))
            (hsPkgs."plutarch-extra" or (errorHandler.buildDepError "plutarch-extra"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."plutarch-test" or (errorHandler.buildDepError "plutarch-test"))
            (hsPkgs."rank2classes" or (errorHandler.buildDepError "rank2classes"))
            ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).lt "9.0") [
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            (hsPkgs."shrinker" or (errorHandler.buildDepError "shrinker"))
            ];
          buildable = true;
          modules = ([
            "BaseSpec"
            "ExtraSpec"
            "Plutarch/ApiSpec"
            "Plutarch/BoolSpec"
            "Plutarch/ByteStringSpec"
            "Plutarch/EitherSpec"
            "Plutarch/IntegerSpec"
            "Plutarch/LiftSpec"
            "Plutarch/ListSpec"
            "Plutarch/ListUtilsSpec"
            "Plutarch/MaybeSpec"
            "Plutarch/PairSpec"
            "Plutarch/PIsDataSpec"
            "Plutarch/PLamSpec"
            "Plutarch/PlutusTypeSpec"
            "Plutarch/POrdSpec"
            "Plutarch/RationalSpec"
            "Plutarch/RecSpec"
            "Plutarch/RecursionSpec"
            "Plutarch/ScriptsSpec"
            "Plutarch/ShowSpec"
            "Plutarch/SpecTypes"
            "Plutarch/StringSpec"
            "Plutarch/Test"
            "Plutarch/Test/Benchmark"
            "Plutarch/Test/Golden"
            "Plutarch/Test/ListSyntax"
            "Plutarch/Test/Property"
            "Plutarch/Test/Property/Extra"
            "Plutarch/Test/Property/Gen"
            "Plutarch/Test/Property/HaskEquiv"
            "Plutarch/Test/Property/Marshal"
            "Plutarch/Test/Run"
            "Plutarch/TraceSpec"
            "Plutarch/UnitSpec"
            "Plutarch/UPLCSpec"
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "9.0") "Plutarch/FFISpec") ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).ge "9.0") [
            "Plutarch/FieldSpec"
            "Plutarch/MonadicSpec"
            "Plutarch/TryFromSpec"
            ];
          hsSourceDirs = [
            "plutarch-base"
            "plutarch-extra"
            "common"
            "conditional"
            "./."
            ];
          mainPath = (([
            "Main.hs"
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "9.0") "") ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "9.0") "") ++ (pkgs.lib).optional (flags.development) "";
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././plutarch-test; }