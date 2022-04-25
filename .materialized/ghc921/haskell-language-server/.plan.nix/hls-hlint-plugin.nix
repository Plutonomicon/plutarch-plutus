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
    flags = { pedantic = false; ghc-lib = false; hlint33 = true; };
    package = {
      specVersion = "2.4";
      identifier = { name = "hls-hlint-plugin"; version = "1.0.3.0"; };
      license = "Apache-2.0";
      copyright = "The Haskell IDE Team";
      maintainer = "atreyu.bbb@gmail.com";
      author = "The Haskell IDE Team";
      homepage = "";
      url = "";
      synopsis = "Hlint integration plugin with Haskell Language Server";
      description = "Please see the README on GitHub at <https://github.com/haskell/haskell-language-server#readme>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "LICENSE"
        "test/testdata/**/*.yaml"
        "test/testdata/ignore/.hlint.yaml"
        "test/testdata/**/*.hs"
        "test/testdata/**/*.h"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."apply-refact" or (errorHandler.buildDepError "apply-refact"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."Diff" or (errorHandler.buildDepError "Diff"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."ghc-exactprint" or (errorHandler.buildDepError "ghc-exactprint"))
          (hsPkgs."ghcide" or (errorHandler.buildDepError "ghcide"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hlint" or (errorHandler.buildDepError "hlint"))
          (hsPkgs."hls-plugin-api" or (errorHandler.buildDepError "hls-plugin-api"))
          (hsPkgs."hslogger" or (errorHandler.buildDepError "hslogger"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."lsp" or (errorHandler.buildDepError "lsp"))
          (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."ghc-lib-parser-ex" or (errorHandler.buildDepError "ghc-lib-parser-ex"))
          ] ++ (if flags.hlint33
          then [
            (hsPkgs."hlint" or (errorHandler.buildDepError "hlint"))
            ] ++ (if !flags.ghc-lib && (compiler.isGhc && (compiler.version).ge "9.0.1") && (compiler.isGhc && (compiler.version).lt "9.1.0")
            then [ (hsPkgs."ghc" or (errorHandler.buildDepError "ghc")) ]
            else [
              (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
              (hsPkgs."ghc-lib" or (errorHandler.buildDepError "ghc-lib"))
              (hsPkgs."ghc-lib-parser-ex" or (errorHandler.buildDepError "ghc-lib-parser-ex"))
              ])
          else [
            (hsPkgs."hlint" or (errorHandler.buildDepError "hlint"))
            ] ++ (if !flags.ghc-lib && (compiler.isGhc && (compiler.version).ge "8.10.1") && (compiler.isGhc && (compiler.version).lt "8.11.0")
            then [ (hsPkgs."ghc" or (errorHandler.buildDepError "ghc")) ]
            else [
              (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
              (hsPkgs."ghc-lib" or (errorHandler.buildDepError "ghc-lib"))
              (hsPkgs."ghc-lib-parser-ex" or (errorHandler.buildDepError "ghc-lib-parser-ex"))
              ]));
        buildable = true;
        modules = [ "Ide/Plugin/Hlint" ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hls-hlint-plugin" or (errorHandler.buildDepError "hls-hlint-plugin"))
            (hsPkgs."hls-plugin-api" or (errorHandler.buildDepError "hls-plugin-api"))
            (hsPkgs."hls-test-utils" or (errorHandler.buildDepError "hls-test-utils"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."lsp-types" or (errorHandler.buildDepError "lsp-types"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././plugins/hls-hlint-plugin; }