{
  description = "plutarch";

  inputs.haskell-nix.url = "github:L-as/haskell.nix?ref=master";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  inputs.flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";

  # https://github.com/input-output-hk/plutus/pull/4328
  inputs.plutus.url = "github:L-as/plutus?ref=master";
  # https://github.com/input-output-hk/cardano-prelude/pull/162
  inputs.cardano-prelude.url = "github:locallycompact/cardano-prelude?rev=93f95047bb36a055bdd56fb0cafd887c072cdce2";
  inputs.cardano-prelude.flake = false;
  inputs.cardano-base.url = "github:input-output-hk/cardano-base";
  inputs.cardano-base.flake = false;
  inputs.cardano-crypto.url = "github:input-output-hk/cardano-crypto?rev=07397f0e50da97eaa0575d93bee7ac4b2b2576ec";
  inputs.cardano-crypto.flake = false;
  # https://github.com/Quid2/flat/pull/27
  inputs.flat.url = "github:Quid2/flat?rev=41a040c413351e021982bb78bd00f750628f8060";
  inputs.flat.flake = false;
  # https://github.com/input-output-hk/Win32-network/pull/10
  inputs.Win32-network.url = "github:input-output-hk/Win32-network?rev=2d1a01c7cbb9f68a1aefe2934aad6c70644ebfea";
  inputs.Win32-network.flake = false;
  # https://github.com/haskell-foundation/foundation/pull/555
  inputs.foundation.url = "github:haskell-foundation/foundation?rev=0bb195e1fea06d144dafc5af9a0ff79af0a5f4a0";
  inputs.foundation.flake = false;
  # https://github.com/locallycompact/protolude
  inputs.protolude.url = "github:protolude/protolude?rev=d821ef0ac7552cfa2c3e7a7bdf29539f57e3fae6";
  inputs.protolude.flake = false;
  # https://github.com/vincenthz/hs-memory/pull/87
  inputs.hs-memory.url = "github:vincenthz/hs-memory?rev=3cf661a8a9a8ac028df77daa88e8d65c55a3347a";
  inputs.hs-memory.flake = false;
  # https://github.com/haskell-crypto/cryptonite/issues/357
  inputs.cryptonite.url = "github:haskell-crypto/cryptonite?rev=cec291d988f0f17828384f3358214ab9bf724a13";
  inputs.cryptonite.flake = false;
  # https://github.com/JonasDuregard/sized-functors/pull/10
  inputs.sized-functors.url = "github:JonasDuregard/sized-functors?rev=fe6bf78a1b97ff7429630d0e8974c9bc40945dcf";
  inputs.sized-functors.flake = false;
  # https://github.com/mokus0/th-extras/pull/17
  inputs.th-extras.url = "github:mokus0/th-extras?rev=787ed752c1e5d41b5903b74e171ed087de38bffa";
  inputs.th-extras.flake = false;
  inputs.Shrinker.url = "github:Plutonomicon/Shrinker";
  inputs.Shrinker.flake = false;
  inputs.haskell-language-server.url = "github:haskell/haskell-language-server";
  inputs.haskell-language-server.flake = false;

  outputs = inputs@{ self, nixpkgs, haskell-nix, plutus, flake-compat-ci, ... }:
    let
      extraSources = [
        {
          src = inputs.protolude;
          subdirs = [ "." ];
        }
        {
          src = inputs.foundation;
          subdirs = [
            "foundation"
            "basement"
          ];
        }
        {
          src = inputs.cardano-prelude;
          subdirs = [
            "cardano-prelude"
          ];
        }
        {
          src = inputs.hs-memory;
          subdirs = [ "." ];
        }
        {
          src = inputs.cardano-crypto;
          subdirs = [ "." ];
        }
        {
          src = inputs.cryptonite;
          subdirs = [ "." ];
        }
        {
          src = inputs.flat;
          subdirs = [ "." ];
        }
        {
          src = inputs.cardano-base;
          subdirs = [
            "binary"
            "cardano-crypto-class"
          ];
        }
        {
          src = inputs.sized-functors;
          subdirs = [ "." ];
        }
        {
          src = inputs.th-extras;
          subdirs = [ "." ];
        }
        {
          src = inputs.plutus;
          subdirs = [
            "plutus-core"
            "plutus-ledger-api"
            "plutus-tx"
            "prettyprinter-configurable"
            "word-array"
          ];
        }
      ];

      supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs { inherit system; overlays = [ haskell-nix.overlay ]; inherit (haskell-nix) config; };
      nixpkgsFor' = system: import nixpkgs { inherit system; inherit (haskell-nix) config; };

      ghcVersion = "ghc921";

      tools.fourmolu = { };
      tools.haskell-language-server = {
        version = "latest";
        modules = [{
          packages = {
            haskell-language-server = {
              src = "${inputs.haskell-language-server}";
              flags = {
                pedantic = true;
                ignore-plugins-ghc-bounds = true;
                alternateNumberFormat = false;
                brittany = false;
                callhierarchy = false;
                class = false;
                eval = false;
                floskell = false;
                fourmolu = false;
                haddockComments = false;
                hlint = false;
                importLens = false;
                ormolu = false;
                refineImports = false;
                retrie = false;
                splice = false;
                stylishhaskell = false;
                tactic = false;
              };
            };
            hie-compat.src = "${inputs.haskell-language-server}/hie-compat";
            hls-graph.src = "${inputs.haskell-language-server}/hls-graph";
            ghcide.src = "${inputs.haskell-language-server}/ghcide";
            hls-plugin-api.src = "${inputs.haskell-language-server}/hls-plugin-api";
            hls-test-utils.src = "${inputs.haskell-language-server}/hls-test-utils";
            shake-bench.src = "${inputs.haskell-language-server}/shake-bench";
            hls-call-hierarchy-plugin.src = "${inputs.haskell-language-server}/plugins/hls-call-hierarchy-plugin";
            hls-class-plugins.src = "${inputs.haskell-language-server}/plugins/hls-class-plugins";
            hls-explicit-imports-plugin.src = "${inputs.haskell-language-server}/plugins/hls-explicit-imports-plugin";
            hls-qualify-imported-names-plugin.src = "${inputs.haskell-language-server}/plugins/hls-qualify-imported-names-plugin";
            hls-pragmas-plugin.src = "${inputs.haskell-language-server}/plugins/hls-pragmas-plugin";
            hls-module-name-plugin.src = "${inputs.haskell-language-server}/plugins/hls-module-name-plugin";
          };
        }];
      };

      projectFor = system:
        let pkgs = nixpkgsFor system; in
        let pkgs' = nixpkgsFor' system; in
        (nixpkgsFor system).haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = ghcVersion;
          cabalProjectFileName = "cabal.project";
          inherit extraSources;
          modules = [{
            packages = {
              basement.src = "${inputs.foundation}/basement";
              basement.components.library.postUnpack = "\n";
              cardano-binary.doHaddock = false;
              cardano-binary.ghcOptions = [ "-Wwarn" ];
              cardano-binary.src = "${inputs.cardano-base}/binary";
              cardano-binary.components.library.postUnpack = "\n";
              cardano-crypto-class.components.library.pkgconfig = nixpkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
              cardano-crypto-class.doHaddock = false;
              cardano-crypto-class.ghcOptions = [ "-Wwarn" ];
              cardano-crypto-class.src = "${inputs.cardano-base}/cardano-crypto-class";
              cardano-crypto-class.components.library.postUnpack = "\n";
              cardano-crypto-praos.components.library.pkgconfig = nixpkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
              cardano-crypto.src = "${inputs.cardano-crypto}";
              cardano-crypto.components.library.postUnpack = "\n";
              cardano-prelude.doHaddock = false; # somehow above options are not applied?
              cardano-prelude.ghcOptions = [ "-Wwarn" ];
              cardano-prelude.src = "${inputs.cardano-prelude}/cardano-prelude";
              cardano-prelude.components.library.postUnpack = "\n";
              cryptonite.src = "${inputs.cryptonite}";
              cryptonite.components.library.postUnpack = "\n";
              flat.src = "${inputs.flat}";
              flat.components.library.postUnpack = "\n";
              foundation.src = "${inputs.foundation}/foundation";
              foundation.components.library.postUnpack = "\n";
              memory.src = "${inputs.hs-memory}";
              memory.components.library.postUnpack = "\n";
              plutus-core.src = "${inputs.plutus}/plutus-core";
              plutus-core.components.library.postUnpack = "\n";
              plutus-tx.src = "${inputs.plutus}/plutus-tx";
              plutus-tx.components.library.postUnpack = "\n";
              plutus-ledger-api.src = "${inputs.plutus}/plutus-ledger-api";
              plutus-ledger-api.components.library.postUnpack = "\n";
              #prettyprinter-configurable.src = "${inputs.plutus}/prettyprinter-configurable";
              #prettyprinter-configurable.components.library.postUnpack = "\n";
              protolude.src = "${inputs.protolude}";
              protolude.components.library.postUnpack = "\n";
              word-array.src = "${inputs.plutus}/word-array";
              word-array.components.library.postUnpack = "\n";
            };
          }];
          shell = {
            withHoogle = true;

            exactDeps = true;

            # We use the ones from Nixpkgs, since they are cached reliably.
            # Eventually we will probably want to build these with haskell.nix.
            nativeBuildInputs = [ pkgs'.cabal-install pkgs'.hlint pkgs'.haskellPackages.cabal-fmt pkgs'.nixpkgs-fmt ];

            inherit tools;

            additional = ps: [
              ps.plutus-ledger-api
              #ps.shrinker
              #ps.shrinker-testing
            ];
          };
        };

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          t = pkgs.haskell-nix.tools ghcVersion { inherit (tools) fourmolu haskell-language-server; };
        in
        pkgs.runCommand "format-check"
          {
            nativeBuildInputs = [ pkgs'.haskellPackages.cabal-fmt pkgs'.nixpkgs-fmt t.fourmolu ];
          } ''
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          cd ${self}
          ./bin/format || (echo "    Please run ./bin/format" ; exit 1)
          mkdir $out
        ''
      ;
    in
    {
      inherit extraSources;

      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      packages = perSystem (system: self.flake.${system}.packages);
      checks = perSystem (system:
        self.flake.${system}.checks
        // {
          formatCheck = formatCheckFor system;
          benchmark = (nixpkgsFor system).runCommand "benchmark" { } "${self.apps.${system}.benchmark.program} | tee $out";
        }
      );
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test"
          {
            checksss = builtins.attrValues self.checks.${system};
          } ''
          echo $checksss

          touch $out
        ''
      );
      apps = perSystem (system:
        self.flake.${system}.apps
        // {
          benchmark = {
            type = "app";
            program = "${self.flake.${system}.packages."plutarch:bench:perf"}/bin/perf";
          };
        }
      );
      devShell = perSystem (system: self.flake.${system}.devShell);

      nixCi = flake-compat-ci.lib.recurseIntoFlakeWith {
        flake = self;
        systems = [ "x86_64-linux" ];
      };
    };
}
