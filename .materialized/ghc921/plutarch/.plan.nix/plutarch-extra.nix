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
      specVersion = "2.4";
      identifier = { name = "plutarch-extra"; version = "1.1.0"; };
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
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."plutarch" or (errorHandler.buildDepError "plutarch"))
          ];
        buildable = true;
        modules = [
          "Plutarch/Extra"
          "Plutarch/Extra/Api"
          "Plutarch/Extra/TermCont"
          "Plutarch/ListUtils"
          ];
        };
      sublibs = {
        "plutarch-preludes" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutarch" or (errorHandler.buildDepError "plutarch"))
            ];
          buildable = true;
          modules = [ "PPrelude" ];
          hsSourceDirs = [ "preludes" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././plutarch-extra; }