{
  description = "plutarch";

  inputs.haskell-nix.url = "github:input-output-hk/haskell.nix?rev=4aeeba8d713d0b98c92c8c717df24da17d463c1d";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  inputs.haskell-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs-2105";
  inputs.flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";

  inputs.plutus.url = "github:input-output-hk/plutus?rev=6d8d25d1e84b2a4278da1036aab23da4161b8df8";
  inputs.cardano-prelude.url = "github:input-output-hk/cardano-prelude?rev=fd773f7a58412131512b9f694ab95653ac430852";
  inputs.cardano-prelude.flake = false;
  inputs.cardano-base.url = "github:input-output-hk/cardano-base?rev=0b1b5b37e305c4bb10791f843bc8c81686a0cba4";
  inputs.cardano-base.flake = false;
  inputs.cardano-crypto.url = "github:input-output-hk/cardano-crypto?rev=07397f0e50da97eaa0575d93bee7ac4b2b2576ec";
  inputs.cardano-crypto.flake = false;
  inputs.flat.url = "github:Quid2/flat?rev=d32c2c0c0c3c38c41177684ade9febe92d279b06";
  inputs.flat.flake = false;
  inputs.Win32-network.url = "github:input-output-hk/Win32-network?rev=3825d3abf75f83f406c1f7161883c438dac7277d";
  inputs.Win32-network.flake = false;
  inputs.Shrinker.url = "github:Plutonomicon/Shrinker";
  inputs.flake = false;

  outputs = inputs@{ self, nixpkgs, haskell-nix, plutus, flake-compat-ci, ... }:
    let
      supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs { inherit system; overlays = [ haskell-nix.overlay ]; inherit (haskell-nix) config; };

      projectFor = system:
        let pkgs = nixpkgsFor system; in
        (nixpkgsFor system).haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc8107";
          cabalProjectFileName = "cabal.project";
          extraSources = [
            {
              src = inputs.cardano-prelude;
              subdirs = [
                "cardano-prelude"
                "cardano-prelude-test"
              ];
            }
            {
              src = inputs.cardano-base;
              subdirs = [
                "base-deriving-via"
                "binary"
                "binary/test"
                "cardano-crypto-class"
                "cardano-crypto-praos"
                "cardano-crypto-tests"
                "measures"
                "orphans-deriving-via"
                "slotting"
                "strict-containers"
              ];
            }
            {
              src = inputs.cardano-crypto;
              subdirs = [ "." ];
            }
            {
              src = inputs.Win32-network;
              subdirs = [ "." ];
            }
            {
              src = inputs.flat;
              subdirs = [ "." ];
            }
            {
              src = inputs.plutus;
              subdirs = [
                "plutus-benchmark"
                "plutus-core"
                "plutus-errors"
                "plutus-ledger-api"
                "plutus-metatheory"
                "plutus-tx"
                "plutus-tx-plugin"
                "prettyprinter-configurable"
                "word-array"
                "stubs/plutus-ghc-stub"
              ];
            }
            {
              src = inputs.Shrinker;
              subdirs = [ "." "testing" ];
            }
          ];
          modules = [{
            packages = {
              cardano-crypto-praos.components.library.pkgconfig =
                nixpkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
              cardano-crypto-class.components.library.pkgconfig =
                nixpkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
            };
          }];
          shell = {
            withHoogle = true;

            exactDeps = true;

            # We use the ones from Nixpkgs, since they are cached reliably.
            # Eventually we will probably want to build these with haskell.nix.
            nativeBuildInputs = [ pkgs.cabal-install pkgs.hlint pkgs.haskellPackages.fourmolu ];

            tools = {
              haskell-language-server = {};  # Must use haskell.nix, because the compiler version should match
            };

            additional = ps: [
              ps.plutus-ledger-api
              ps.shrinker
              ps.shrinker-testing
            ];
          };
        };

        formatCheckFor = system:
          let
            pkgs = nixpkgsFor system;
          in
            pkgs.runCommand "format-check" {
              nativeBuildInputs = [ pkgs.haskellPackages.fourmolu ];
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
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake {});

      # this could be done automatically, but would reduce readability
      packages = perSystem (system: self.flake.${system}.packages);
      checks = perSystem (system:
        self.flake.${system}.checks
        // {
          formatCheck = formatCheckFor system;
        }
      );
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test" {
          nativeBuildInputs = builtins.attrValues self.checks.${system};
        } "touch $out"
      );
      apps = perSystem (system: self.flake.${system}.apps);
      devShell = perSystem (system: self.flake.${system}.devShell);

      nixCi = flake-compat-ci.lib.recurseIntoFlakeWith {
        flake = self;
        systems = [ "x86_64-linux" ];
      };
    };
}
