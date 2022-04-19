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
      support_bytestring = true;
      support_foundation = true;
      support_basement = true;
      support_deepseq = true;
      };
    package = {
      specVersion = "1.18";
      identifier = { name = "memory"; version = "0.16.0"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "vincent@snarc.org, Nicolas Di Prima <nicolas@primetype.co.uk>";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "https://github.com/vincenthz/hs-memory";
      url = "";
      synopsis = "memory and related abstraction stuff";
      description = "Chunk of memory, polymorphic byte array management and manipulation\n\n* A polymorphic byte array abstraction and function similar to strict ByteString.\n\n* Different type of byte array abstraction.\n\n* Raw memory IO operations (memory set, memory copy, ..)\n\n* Aliasing with endianness support.\n\n* Encoding : Base16, Base32, Base64.\n\n* Hashing : FNV, SipHash";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [ "README.md" "CHANGELOG.md" ];
      };
    components = {
      "library" = {
        depends = (((pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).lt "8.0")) [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ] ++ (pkgs.lib).optional (flags.support_bytestring) (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))) ++ (pkgs.lib).optional (flags.support_deepseq) (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))) ++ (pkgs.lib).optional (flags.support_foundation || flags.support_basement) (hsPkgs."basement" or (errorHandler.buildDepError "basement"));
        buildable = if compiler.isGhc && (compiler.version).lt "8.0"
          then false
          else true;
        modules = ([
          "Data/Memory/Internal/Compat"
          "Data/Memory/Internal/CompatPrim"
          "Data/Memory/Internal/CompatPrim64"
          "Data/Memory/Internal/DeepSeq"
          "Data/Memory/Internal/Imports"
          "Data/Memory/Hash/SipHash"
          "Data/Memory/Hash/FNV"
          "Data/Memory/HeadHackageUtils"
          "Data/ByteArray/Pack/Internal"
          "Data/ByteArray/Types"
          "Data/ByteArray/Bytes"
          "Data/ByteArray/ScrubbedBytes"
          "Data/ByteArray/Methods"
          "Data/ByteArray/MemView"
          "Data/ByteArray/View"
          "Data/ByteArray"
          "Data/ByteArray/Encoding"
          "Data/ByteArray/Mapping"
          "Data/ByteArray/Pack"
          "Data/ByteArray/Parse"
          "Data/ByteArray/Hash"
          "Data/Memory/Endian"
          "Data/Memory/PtrMethods"
          "Data/Memory/ExtendedWords"
          "Data/Memory/Encoding/Base16"
          "Data/Memory/Encoding/Base32"
          "Data/Memory/Encoding/Base64"
          ] ++ (if system.isWindows
          then [ "Data/Memory/MemMap/Windows" ]
          else [
            "Data/Memory/MemMap/Posix"
            ])) ++ (pkgs.lib).optional (flags.support_foundation || flags.support_basement) "Data/ByteArray/Sized";
        };
      tests = {
        "test-memory" = {
          depends = [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
            (hsPkgs."foundation" or (errorHandler.buildDepError "foundation"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).lt "8.0")) (hsPkgs."base" or (errorHandler.buildDepError "base"));
          buildable = if compiler.isGhc && (compiler.version).lt "8.0"
            then false
            else true;
          modules = [ "Imports" "SipHash" "Utils" ];
          hsSourceDirs = [ "tests" ];
          mainPath = [ "Tests.hs" ];
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
    postUnpack = "sourceRoot+=/dep3/.; echo source root reset to $sourceRoot";
    }