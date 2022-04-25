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
    flags = { pedantic = false; };
    package = {
      specVersion = "2.4";
      identifier = { name = "hls-tactics-plugin"; version = "1.6.1.0"; };
      license = "Apache-2.0";
      copyright = "Sandy Maguire, Reed Mullanix";
      maintainer = "sandy@sandymaguire.me";
      author = "Sandy Maguire, Reed Mullanix";
      homepage = "https://haskellwingman.dev";
      url = "";
      synopsis = "Wingman plugin for Haskell Language Server";
      description = "Please see the README on GitHub at <https://github.com/haskell/haskell-language-server#readme>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "README.md"
        "test/golden/*.cabal"
        "test/golden/*.hs"
        "test/golden/*.yaml"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."fingertree" or (errorHandler.buildDepError "fingertree"))
          (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
          (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
          (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
          (hsPkgs."ghc-exactprint" or (errorHandler.buildDepError "ghc-exactprint"))
          (hsPkgs."ghc-source-gen" or (errorHandler.buildDepError "ghc-source-gen"))
          (hsPkgs."ghcide" or (errorHandler.buildDepError "ghcide"))
          (hsPkgs."hls-graph" or (errorHandler.buildDepError "hls-graph"))
          (hsPkgs."hls-plugin-api" or (errorHandler.buildDepError "hls-plugin-api"))
          (hsPkgs."hyphenation" or (errorHandler.buildDepError "hyphenation"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."lsp" or (errorHandler.buildDepError "lsp"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."refinery" or (errorHandler.buildDepError "refinery"))
          (hsPkgs."retrie" or (errorHandler.buildDepError "retrie"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."unagi-chan" or (errorHandler.buildDepError "unagi-chan"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        modules = [
          "Ide/Plugin/Tactic"
          "Refinery/Future"
          "Wingman/AbstractLSP"
          "Wingman/AbstractLSP/TacticActions"
          "Wingman/AbstractLSP/Types"
          "Wingman/Auto"
          "Wingman/CaseSplit"
          "Wingman/CodeGen"
          "Wingman/CodeGen/Utils"
          "Wingman/Context"
          "Wingman/Debug"
          "Wingman/EmptyCase"
          "Wingman/GHC"
          "Wingman/Judgements"
          "Wingman/Judgements/SYB"
          "Wingman/Judgements/Theta"
          "Wingman/KnownStrategies"
          "Wingman/KnownStrategies/QuickCheck"
          "Wingman/LanguageServer"
          "Wingman/LanguageServer/Metaprogram"
          "Wingman/LanguageServer/TacticProviders"
          "Wingman/Machinery"
          "Wingman/Metaprogramming/Lexer"
          "Wingman/Metaprogramming/Parser"
          "Wingman/Metaprogramming/Parser/Documentation"
          "Wingman/Metaprogramming/ProofState"
          "Wingman/Naming"
          "Wingman/Plugin"
          "Wingman/Range"
          "Wingman/Simplify"
          "Wingman/StaticPlugin"
          "Wingman/Tactics"
          "Wingman/Types"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            (hsPkgs."ghcide" or (errorHandler.buildDepError "ghcide"))
            (hsPkgs."hls-plugin-api" or (errorHandler.buildDepError "hls-plugin-api"))
            (hsPkgs."hls-tactics-plugin" or (errorHandler.buildDepError "hls-tactics-plugin"))
            (hsPkgs."hls-test-utils" or (errorHandler.buildDepError "hls-test-utils"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-expectations" or (errorHandler.buildDepError "hspec-expectations"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."lsp-types" or (errorHandler.buildDepError "lsp-types"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty-hspec" or (errorHandler.buildDepError "tasty-hspec"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          modules = [
            "AutoTupleSpec"
            "CodeAction/AutoSpec"
            "CodeAction/DestructAllSpec"
            "CodeAction/DestructPunSpec"
            "CodeAction/DestructSpec"
            "CodeAction/IntrosSpec"
            "CodeAction/IntroDestructSpec"
            "CodeAction/RefineSpec"
            "CodeAction/RunMetaprogramSpec"
            "CodeAction/UseDataConSpec"
            "CodeLens/EmptyCaseSpec"
            "ProviderSpec"
            "Spec"
            "UnificationSpec"
            "Utils"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././plugins/hls-tactics-plugin; }