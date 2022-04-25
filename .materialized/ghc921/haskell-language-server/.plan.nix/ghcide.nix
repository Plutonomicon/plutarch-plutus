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
    flags = {
      ghc-patched-unboxed-bytecode = false;
      test-exe = true;
      executable = true;
      bench-exe = true;
      };
    package = {
      specVersion = "2.4";
      identifier = { name = "ghcide"; version = "1.6.0.1"; };
      license = "Apache-2.0";
      copyright = "Digital Asset and Ghcide contributors 2018-2020";
      maintainer = "Ghcide contributors";
      author = "Digital Asset and Ghcide contributors";
      homepage = "https://github.com/haskell/haskell-language-server/tree/master/ghcide#readme";
      url = "";
      synopsis = "The core of an IDE";
      description = "A library for building Haskell IDE's on top of the GHC API.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "README.md"
        "CHANGELOG.md"
        "test/data/**/*.project"
        "test/data/**/*.cabal"
        "test/data/**/*.yaml"
        "test/data/**/*.hs"
        "test/data/**/*.hs-boot"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."dependent-map" or (errorHandler.buildDepError "dependent-map"))
          (hsPkgs."dependent-sum" or (errorHandler.buildDepError "dependent-sum"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."fuzzy" or (errorHandler.buildDepError "fuzzy"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."fingertree" or (errorHandler.buildDepError "fingertree"))
          (hsPkgs."focus" or (errorHandler.buildDepError "focus"))
          (hsPkgs."ghc-exactprint" or (errorHandler.buildDepError "ghc-exactprint"))
          (hsPkgs."ghc-trace-events" or (errorHandler.buildDepError "ghc-trace-events"))
          (hsPkgs."Glob" or (errorHandler.buildDepError "Glob"))
          (hsPkgs."haddock-library" or (errorHandler.buildDepError "haddock-library"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hie-compat" or (errorHandler.buildDepError "hie-compat"))
          (hsPkgs."hls-plugin-api" or (errorHandler.buildDepError "hls-plugin-api"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."list-t" or (errorHandler.buildDepError "list-t"))
          (hsPkgs."hiedb" or (errorHandler.buildDepError "hiedb"))
          (hsPkgs."lsp-types" or (errorHandler.buildDepError "lsp-types"))
          (hsPkgs."lsp" or (errorHandler.buildDepError "lsp"))
          (hsPkgs."monoid-subclasses" or (errorHandler.buildDepError "monoid-subclasses"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
          (hsPkgs."prettyprinter-ansi-terminal" or (errorHandler.buildDepError "prettyprinter-ansi-terminal"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
          (hsPkgs."retrie" or (errorHandler.buildDepError "retrie"))
          (hsPkgs."rope-utf16-splay" or (errorHandler.buildDepError "rope-utf16-splay"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."hls-graph" or (errorHandler.buildDepError "hls-graph"))
          (hsPkgs."sorted-list" or (errorHandler.buildDepError "sorted-list"))
          (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."stm-containers" or (errorHandler.buildDepError "stm-containers"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."vector-algorithms" or (errorHandler.buildDepError "vector-algorithms"))
          (hsPkgs."hslogger" or (errorHandler.buildDepError "hslogger"))
          (hsPkgs."Diff" or (errorHandler.buildDepError "Diff"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."opentelemetry" or (errorHandler.buildDepError "opentelemetry"))
          (hsPkgs."heapsize" or (errorHandler.buildDepError "heapsize"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
          (hsPkgs."ghc-boot" or (errorHandler.buildDepError "ghc-boot"))
          (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
          (hsPkgs."ghc-check" or (errorHandler.buildDepError "ghc-check"))
          (hsPkgs."ghc-paths" or (errorHandler.buildDepError "ghc-paths"))
          (hsPkgs."cryptohash-sha1" or (errorHandler.buildDepError "cryptohash-sha1"))
          (hsPkgs."hie-bios" or (errorHandler.buildDepError "hie-bios"))
          (hsPkgs."implicit-hie-cradle" or (errorHandler.buildDepError "implicit-hie-cradle"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        buildable = true;
        modules = [
          "Development/IDE/Core/FileExists"
          "Development/IDE/GHC/CPP"
          "Development/IDE/GHC/Warnings"
          "Development/IDE/LSP/Notifications"
          "Development/IDE/Plugin/CodeAction/PositionIndexed"
          "Development/IDE/Plugin/CodeAction/Args"
          "Development/IDE/Plugin/Completions/Logic"
          "Development/IDE/Session/VersionCheck"
          "Development/IDE/Types/Action"
          "Text/Fuzzy/Parallel"
          "Control/Concurrent/Strict"
          "Generics/SYB/GHC"
          "Development/IDE"
          "Development/IDE/Main"
          "Development/IDE/Core/Actions"
          "Development/IDE/Main/HeapStats"
          "Development/IDE/Core/Debouncer"
          "Development/IDE/Core/FileStore"
          "Development/IDE/Core/IdeConfiguration"
          "Development/IDE/Core/OfInterest"
          "Development/IDE/Core/PositionMapping"
          "Development/IDE/Core/Preprocessor"
          "Development/IDE/Core/ProgressReporting"
          "Development/IDE/Core/Rules"
          "Development/IDE/Core/RuleTypes"
          "Development/IDE/Core/Service"
          "Development/IDE/Core/Shake"
          "Development/IDE/Core/Tracing"
          "Development/IDE/Core/UseStale"
          "Development/IDE/GHC/Compat"
          "Development/IDE/GHC/Compat/Core"
          "Development/IDE/GHC/Compat/Env"
          "Development/IDE/GHC/Compat/ExactPrint"
          "Development/IDE/GHC/Compat/Iface"
          "Development/IDE/GHC/Compat/Logger"
          "Development/IDE/GHC/Compat/Outputable"
          "Development/IDE/GHC/Compat/Parser"
          "Development/IDE/GHC/Compat/Plugins"
          "Development/IDE/GHC/Compat/Units"
          "Development/IDE/GHC/Compat/Util"
          "Development/IDE/Core/Compile"
          "Development/IDE/GHC/Dump"
          "Development/IDE/GHC/Error"
          "Development/IDE/GHC/ExactPrint"
          "Development/IDE/GHC/Orphans"
          "Development/IDE/GHC/Util"
          "Development/IDE/Import/DependencyInformation"
          "Development/IDE/Import/FindImports"
          "Development/IDE/LSP/HoverDefinition"
          "Development/IDE/LSP/LanguageServer"
          "Development/IDE/LSP/Outline"
          "Development/IDE/LSP/Server"
          "Development/IDE/Session"
          "Development/IDE/Spans/Common"
          "Development/IDE/Spans/Documentation"
          "Development/IDE/Spans/AtPoint"
          "Development/IDE/Spans/LocalBindings"
          "Development/IDE/Spans/Pragmas"
          "Development/IDE/Types/Diagnostics"
          "Development/IDE/Types/Exports"
          "Development/IDE/Types/HscEnvEq"
          "Development/IDE/Types/KnownTargets"
          "Development/IDE/Types/Location"
          "Development/IDE/Types/Logger"
          "Development/IDE/Types/Options"
          "Development/IDE/Types/Shake"
          "Development/IDE/Plugin"
          "Development/IDE/Plugin/Completions"
          "Development/IDE/Plugin/Completions/Types"
          "Development/IDE/Plugin/CodeAction"
          "Development/IDE/Plugin/CodeAction/ExactPrint"
          "Development/IDE/Plugin/HLS"
          "Development/IDE/Plugin/HLS/GhcIde"
          "Development/IDE/Plugin/Test"
          "Development/IDE/Plugin/TypeLenses"
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.10") "Development/IDE/GHC/Compat/CPP";
        hsSourceDirs = [ "src" "session-loader" ];
        };
      exes = {
        "ghcide-test-preprocessor" = {
          depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
          buildable = if !flags.test-exe then false else true;
          hsSourceDirs = [ "test/preprocessor" ];
          mainPath = [ "Main.hs" ] ++ (pkgs.lib).optional (!flags.test-exe) "";
          };
        "ghcide" = {
          depends = [
            (hsPkgs."hiedb" or (errorHandler.buildDepError "hiedb"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."gitrev" or (errorHandler.buildDepError "gitrev"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."lsp" or (errorHandler.buildDepError "lsp"))
            (hsPkgs."lsp-types" or (errorHandler.buildDepError "lsp-types"))
            (hsPkgs."heapsize" or (errorHandler.buildDepError "heapsize"))
            (hsPkgs."hie-bios" or (errorHandler.buildDepError "hie-bios"))
            (hsPkgs."hls-plugin-api" or (errorHandler.buildDepError "hls-plugin-api"))
            (hsPkgs."ghcide" or (errorHandler.buildDepError "ghcide"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."hls-graph" or (errorHandler.buildDepError "hls-graph"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = if !flags.executable then false else true;
          modules = [ "Arguments" "Paths_ghcide" ];
          hsSourceDirs = [ "exe" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (!flags.executable) "";
          };
        "ghcide-bench" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghcide" or (errorHandler.buildDepError "ghcide"))
            (hsPkgs."hls-plugin-api" or (errorHandler.buildDepError "hls-plugin-api"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."lsp-test" or (errorHandler.buildDepError "lsp-test"))
            (hsPkgs."lsp-types" or (errorHandler.buildDepError "lsp-types"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."hls-graph" or (errorHandler.buildDepError "hls-graph"))
            (hsPkgs."shake" or (errorHandler.buildDepError "shake"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.ghcide.components.exes.ghcide or (pkgs.buildPackages.ghcide or (errorHandler.buildToolDepError "ghcide:ghcide")))
            ];
          buildable = if !flags.bench-exe then false else true;
          modules = [
            "Development/IDE/Test"
            "Development/IDE/Test/Diagnostic"
            "Experiments"
            "Experiments/Types"
            ];
          hsSourceDirs = [ "bench/lib" "bench/exe" "test/src" ];
          mainPath = [ "Main.hs" ] ++ (pkgs.lib).optional (!flags.bench-exe) "";
          };
        };
      tests = {
        "ghcide-tests" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            (hsPkgs."ghcide" or (errorHandler.buildDepError "ghcide"))
            (hsPkgs."ghc-typelits-knownnat" or (errorHandler.buildDepError "ghc-typelits-knownnat"))
            (hsPkgs."haddock-library" or (errorHandler.buildDepError "haddock-library"))
            (hsPkgs."lsp" or (errorHandler.buildDepError "lsp"))
            (hsPkgs."lsp-types" or (errorHandler.buildDepError "lsp-types"))
            (hsPkgs."hls-plugin-api" or (errorHandler.buildDepError "hls-plugin-api"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."list-t" or (errorHandler.buildDepError "list-t"))
            (hsPkgs."lsp-test" or (errorHandler.buildDepError "lsp-test"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."rope-utf16-splay" or (errorHandler.buildDepError "rope-utf16-splay"))
            (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
            (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."shake" or (errorHandler.buildDepError "shake"))
            (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."stm-containers" or (errorHandler.buildDepError "stm-containers"))
            (hsPkgs."hls-graph" or (errorHandler.buildDepError "hls-graph"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-rerun" or (errorHandler.buildDepError "tasty-rerun"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).ge "8.6" && (compiler.isGhc && (compiler.version).lt "9.2")) [
            (hsPkgs."record-dot-preprocessor" or (errorHandler.buildDepError "record-dot-preprocessor"))
            (hsPkgs."record-hasfield" or (errorHandler.buildDepError "record-hasfield"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.ghcide.components.exes.ghcide or (pkgs.buildPackages.ghcide or (errorHandler.buildToolDepError "ghcide:ghcide")))
            (hsPkgs.buildPackages.ghcide.components.exes.ghcide-test-preprocessor or (pkgs.buildPackages.ghcide-test-preprocessor or (errorHandler.buildToolDepError "ghcide:ghcide-test-preprocessor")))
            (hsPkgs.buildPackages.implicit-hie.components.exes.gen-hie or (pkgs.buildPackages.gen-hie or (errorHandler.buildToolDepError "implicit-hie:gen-hie")))
            ];
          buildable = true;
          modules = [
            "Development/IDE/Test"
            "Development/IDE/Test/Diagnostic"
            "Development/IDE/Test/Runfiles"
            "Experiments"
            "Experiments/Types"
            "Progress"
            "HieDbRetry"
            ];
          hsSourceDirs = [ "test/cabal" "test/exe" "test/src" "bench/lib" ];
          mainPath = [ "Main.hs" ];
          };
        };
      benchmarks = {
        "benchHist" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."shake-bench" or (errorHandler.buildDepError "shake-bench"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."shake" or (errorHandler.buildDepError "shake"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.ghcide.components.exes.ghcide-bench or (pkgs.buildPackages.ghcide-bench or (errorHandler.buildToolDepError "ghcide:ghcide-bench")))
            (hsPkgs.buildPackages.hp2pretty.components.exes.hp2pretty or (pkgs.buildPackages.hp2pretty or (errorHandler.buildToolDepError "hp2pretty:hp2pretty")))
            (hsPkgs.buildPackages.implicit-hie.components.exes.gen-hie or (pkgs.buildPackages.gen-hie or (errorHandler.buildToolDepError "implicit-hie:gen-hie")))
            ];
          buildable = true;
          modules = [ "Experiments/Types" ];
          hsSourceDirs = [ "bench/hist" "bench/lib" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././ghcide; }