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
    flags = {};
    package = {
      specVersion = "3.0";
      identifier = { name = "shrinker"; version = "0.1"; };
      license = "NONE";
      copyright = "TODO";
      maintainer = "brian@mlabs.city";
      author = "MLabs";
      homepage = "https://github.com/Geometer1729/Shrinker";
      url = "";
      synopsis = "TODO";
      description = "Tools for optimizing uplc script sizes";
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
          (hsPkgs."record-dot-preprocessor" or (errorHandler.buildDepError "record-dot-preprocessor"))
          (hsPkgs."record-hasfield" or (errorHandler.buildDepError "record-hasfield"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        modules = [
          "Shrink/ScopeM"
          "Shrink/Tactics/Util"
          "Shrink"
          "Shrink/Types"
          "Shrink/Names"
          "Shrink/Tactics/Safe"
          "Shrink/Tactics/Tactics"
          ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "0";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "0";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/dep14/.; echo source root reset to $sourceRoot";
    }