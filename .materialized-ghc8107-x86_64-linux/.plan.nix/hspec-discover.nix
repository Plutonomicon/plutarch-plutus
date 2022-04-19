{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.12";
      identifier = { name = "hspec-discover"; version = "2.9.5"; };
      license = "MIT";
      copyright = "(c) 2012-2021 Simon Hengel";
      maintainer = "Simon Hengel <sol@typeful.net>";
      author = "Simon Hengel <sol@typeful.net>";
      homepage = "http://hspec.github.io/";
      url = "";
      synopsis = "Automatically discover and run Hspec tests";
      description = "Automatically discover and run Hspec tests\n\n<http://hspec.github.io/hspec-discover.html>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "version.yaml" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          ];
        buildable = true;
        modules = [
          "Paths_hspec_discover"
          "Test/Hspec/Discover/Config"
          "Test/Hspec/Discover/Run"
          "Test/Hspec/Discover/Sort"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "hspec-discover" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec-discover" or (errorHandler.buildDepError "hspec-discover"))
            ];
          buildable = true;
          modules = [ "Paths_hspec_discover" ];
          hsSourceDirs = [ "driver" ];
          mainPath = [ "hspec-discover.hs" ];
          };
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec-discover" or (errorHandler.buildDepError "hspec-discover"))
            (hsPkgs."hspec-meta" or (errorHandler.buildDepError "hspec-meta"))
            (hsPkgs."mockery" or (errorHandler.buildDepError "mockery"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-meta.components.exes.hspec-meta-discover or (pkgs.buildPackages.hspec-meta-discover or (errorHandler.buildToolDepError "hspec-meta:hspec-meta-discover")))
            ];
          buildable = true;
          modules = [
            "Helper"
            "Test/Hspec/Discover/ConfigSpec"
            "Test/Hspec/Discover/RunSpec"
            "Test/Hspec/Discover/SortSpec"
            "Paths_hspec_discover"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
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
    postUnpack = "sourceRoot+=/dep11/hspec-discover; echo source root reset to $sourceRoot";
    }) // { cabal-generator = "hpack"; }