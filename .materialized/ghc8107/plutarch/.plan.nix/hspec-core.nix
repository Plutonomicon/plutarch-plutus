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
      identifier = { name = "hspec-core"; version = "2.9.5"; };
      license = "MIT";
      copyright = "(c) 2011-2021 Simon Hengel,\n(c) 2011-2012 Trystan Spangler,\n(c) 2011 Greg Weber";
      maintainer = "Simon Hengel <sol@typeful.net>";
      author = "";
      homepage = "http://hspec.github.io/";
      url = "";
      synopsis = "A Testing Framework for Haskell";
      description = "This package exposes internal types and functions that can be used to extend Hspec's functionality.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "version.yaml" "help.txt" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hspec-expectations" or (errorHandler.buildDepError "hspec-expectations"))
          (hsPkgs."quickcheck-io" or (errorHandler.buildDepError "quickcheck-io"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."setenv" or (errorHandler.buildDepError "setenv"))
          (hsPkgs."tf-random" or (errorHandler.buildDepError "tf-random"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).ge "8.2.1") [
          (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
          (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
          ]) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.4.1") (hsPkgs."stm" or (errorHandler.buildDepError "stm"));
        buildable = true;
        modules = [
          "GetOpt/Declarative"
          "GetOpt/Declarative/Environment"
          "GetOpt/Declarative/Interpret"
          "GetOpt/Declarative/Types"
          "GetOpt/Declarative/Util"
          "Test/Hspec/Core/Clock"
          "Test/Hspec/Core/Compat"
          "Test/Hspec/Core/Config"
          "Test/Hspec/Core/Config/Definition"
          "Test/Hspec/Core/Config/Options"
          "Test/Hspec/Core/Example"
          "Test/Hspec/Core/Example/Location"
          "Test/Hspec/Core/FailureReport"
          "Test/Hspec/Core/Formatters/Diff"
          "Test/Hspec/Core/Formatters/Internal"
          "Test/Hspec/Core/Formatters/Pretty"
          "Test/Hspec/Core/Formatters/Pretty/Parser"
          "Test/Hspec/Core/Formatters/Pretty/Parser/Types"
          "Test/Hspec/Core/Formatters/Pretty/Unicode"
          "Test/Hspec/Core/Formatters/V1/Free"
          "Test/Hspec/Core/Formatters/V1/Monad"
          "Test/Hspec/Core/QuickCheckUtil"
          "Test/Hspec/Core/Runner/Eval"
          "Test/Hspec/Core/Runner/PrintSlowSpecItems"
          "Test/Hspec/Core/Shuffle"
          "Test/Hspec/Core/Spec/Monad"
          "Test/Hspec/Core/Timer"
          "Test/Hspec/Core/Tree"
          "Control/Concurrent/Async"
          "Data/Algorithm/Diff"
          "Paths_hspec_core"
          "Test/Hspec/Core/Spec"
          "Test/Hspec/Core/Hooks"
          "Test/Hspec/Core/Runner"
          "Test/Hspec/Core/Format"
          "Test/Hspec/Core/Formatters"
          "Test/Hspec/Core/Formatters/V1"
          "Test/Hspec/Core/Formatters/V2"
          "Test/Hspec/Core/QuickCheck"
          "Test/Hspec/Core/Util"
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.4.1")) "Control/Concurrent/STM/TMVar";
        hsSourceDirs = [
          "src"
          "vendor"
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.4.1")) "vendor/stm-2.5.0.1/";
        };
      tests = {
        "spec" = {
          depends = ([
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec-expectations" or (errorHandler.buildDepError "hspec-expectations"))
            (hsPkgs."hspec-meta" or (errorHandler.buildDepError "hspec-meta"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."quickcheck-io" or (errorHandler.buildDepError "quickcheck-io"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."setenv" or (errorHandler.buildDepError "setenv"))
            (hsPkgs."silently" or (errorHandler.buildDepError "silently"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."tf-random" or (errorHandler.buildDepError "tf-random"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).ge "8.2.1") [
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
            ]) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.4.1") (hsPkgs."stm" or (errorHandler.buildDepError "stm"));
          build-tools = [
            (hsPkgs.buildPackages.hspec-meta.components.exes.hspec-meta-discover or (pkgs.buildPackages.hspec-meta-discover or (errorHandler.buildToolDepError "hspec-meta:hspec-meta-discover")))
            ];
          buildable = true;
          modules = [
            "GetOpt/Declarative"
            "GetOpt/Declarative/Environment"
            "GetOpt/Declarative/Interpret"
            "GetOpt/Declarative/Types"
            "GetOpt/Declarative/Util"
            "Test/Hspec/Core/Clock"
            "Test/Hspec/Core/Compat"
            "Test/Hspec/Core/Config"
            "Test/Hspec/Core/Config/Definition"
            "Test/Hspec/Core/Config/Options"
            "Test/Hspec/Core/Example"
            "Test/Hspec/Core/Example/Location"
            "Test/Hspec/Core/FailureReport"
            "Test/Hspec/Core/Format"
            "Test/Hspec/Core/Formatters"
            "Test/Hspec/Core/Formatters/Diff"
            "Test/Hspec/Core/Formatters/Internal"
            "Test/Hspec/Core/Formatters/Pretty"
            "Test/Hspec/Core/Formatters/Pretty/Parser"
            "Test/Hspec/Core/Formatters/Pretty/Parser/Types"
            "Test/Hspec/Core/Formatters/Pretty/Unicode"
            "Test/Hspec/Core/Formatters/V1"
            "Test/Hspec/Core/Formatters/V1/Free"
            "Test/Hspec/Core/Formatters/V1/Monad"
            "Test/Hspec/Core/Formatters/V2"
            "Test/Hspec/Core/Hooks"
            "Test/Hspec/Core/QuickCheck"
            "Test/Hspec/Core/QuickCheckUtil"
            "Test/Hspec/Core/Runner"
            "Test/Hspec/Core/Runner/Eval"
            "Test/Hspec/Core/Runner/PrintSlowSpecItems"
            "Test/Hspec/Core/Shuffle"
            "Test/Hspec/Core/Spec"
            "Test/Hspec/Core/Spec/Monad"
            "Test/Hspec/Core/Timer"
            "Test/Hspec/Core/Tree"
            "Test/Hspec/Core/Util"
            "Control/Concurrent/Async"
            "Data/Algorithm/Diff"
            "All"
            "GetOpt/Declarative/EnvironmentSpec"
            "GetOpt/Declarative/UtilSpec"
            "Helper"
            "Mock"
            "Test/Hspec/Core/ClockSpec"
            "Test/Hspec/Core/CompatSpec"
            "Test/Hspec/Core/Config/DefinitionSpec"
            "Test/Hspec/Core/Config/OptionsSpec"
            "Test/Hspec/Core/ConfigSpec"
            "Test/Hspec/Core/Example/LocationSpec"
            "Test/Hspec/Core/ExampleSpec"
            "Test/Hspec/Core/FailureReportSpec"
            "Test/Hspec/Core/FormatSpec"
            "Test/Hspec/Core/Formatters/DiffSpec"
            "Test/Hspec/Core/Formatters/InternalSpec"
            "Test/Hspec/Core/Formatters/Pretty/ParserSpec"
            "Test/Hspec/Core/Formatters/Pretty/UnicodeSpec"
            "Test/Hspec/Core/Formatters/PrettySpec"
            "Test/Hspec/Core/Formatters/V1Spec"
            "Test/Hspec/Core/Formatters/V2Spec"
            "Test/Hspec/Core/HooksSpec"
            "Test/Hspec/Core/QuickCheckUtilSpec"
            "Test/Hspec/Core/Runner/EvalSpec"
            "Test/Hspec/Core/Runner/PrintSlowSpecItemsSpec"
            "Test/Hspec/Core/RunnerSpec"
            "Test/Hspec/Core/ShuffleSpec"
            "Test/Hspec/Core/SpecSpec"
            "Test/Hspec/Core/TimerSpec"
            "Test/Hspec/Core/UtilSpec"
            "Paths_hspec_core"
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.4.1")) "Control/Concurrent/STM/TMVar";
          hsSourceDirs = [
            "src"
            "vendor"
            "test"
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.4.1")) "vendor/stm-2.5.0.1/";
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
    postUnpack = "sourceRoot+=/dep11/hspec-core; echo source root reset to $sourceRoot";
    }) // { cabal-generator = "hpack"; }