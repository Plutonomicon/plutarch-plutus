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
      specVersion = "1.10";
      identifier = { name = "size-based"; version = "0.1.2.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) Jonas Duregård";
      maintainer = "jonas.duregard@chalmers.se";
      author = "Jonas Duregård";
      homepage = "";
      url = "";
      synopsis = "Sized functors, for size-based enumerations";
      description = "A framework for size-based enumerations. See the module documentations for details. ";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."dictionary-sharing" or (errorHandler.buildDepError "dictionary-sharing"))
          (hsPkgs."testing-type-modifiers" or (errorHandler.buildDepError "testing-type-modifiers"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        modules = [
          "Control/Enumerable/Derive"
          "Control/Sized"
          "Control/Enumerable"
          "Control/Enumerable/Count"
          "Control/Enumerable/Values"
          ];
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
    postUnpack = "sourceRoot+=/dep8/.; echo source root reset to $sourceRoot";
    }