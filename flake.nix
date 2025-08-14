{
  description = "plutarch";

  nixConfig = {
    allow-import-from-derivation = "true";
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix \\[\\e[0;1m\\]plutarch \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
    cores = "1";
    max-jobs = "auto";
    auto-optimise-store = "true";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";

    flake-parts.url = "github:hercules-ci/flake-parts";

    haskell-nix.url = "github:input-output-hk/haskell.nix";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    iohk-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs";

    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";

    herbage.url = "github:seungheonoh/herbage";
  };

  outputs = inputs@{ flake-parts, nixpkgs, haskell-nix, iohk-nix, CHaP, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./nix/pre-commit.nix
        ./nix/hercules-ci.nix
      ];
      debug = true;
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      hercules-ci.github-pages.branch = "staging";

      perSystem = { config, system, lib, self', ... }:
        let
          pkgs =
            import haskell-nix.inputs.nixpkgs {
              inherit system;
              overlays = [
                haskell-nix.overlay
                iohk-nix.overlays.crypto
                iohk-nix.overlays.haskell-nix-crypto
                inputs.herbage.overlays.default
              ];
              inherit (haskell-nix) config;
            };

          herbage = inputs.herbage.lib { inherit pkgs; };

          project = pkgs.haskell-nix.cabalProject' {
            src = ./.;
            compiler-nix-name = "ghc966";
            # NOTE(bladyjoker): Follow https://github.com/input-output-hk/plutus/blob/master/cabal.project
            index-state = "2025-07-30T14:13:57Z";
            inputMap = {
              "https://chap.intersectmbo.org/" = CHaP;
            };
            shell = {
              withHoogle = true;
              withHaddock = true;
              exactDeps = false;
              # TODO(peter-mlabs): Use `apply-refact` for repo wide refactoring `find -name '*.hs' -not -path './dist-*/*' -exec hlint -j --refactor --refactor-options="--inplace" {} +``
              shellHook = config.pre-commit.installationScript;
              nativeBuildInputs = with pkgs; [
                mdbook
                hackage-repo-tool
              ];
              tools = {
                cabal = { };
                haskell-language-server = { };
                hlint = { };
                cabal-fmt = { };
                fourmolu = { };
                hspec-discover = { };
                markdown-unlit = { };
              };
            };
          };
          flake = project.flake { };
        in
        {
          inherit (flake) devShells;
          hercules-ci.github-pages.settings.contents = self'.packages.combined-docs;
          packages = flake.packages // {
            plutarch-docs = pkgs.stdenv.mkDerivation {
              name = "pluatrch-docs";
              src = ./plutarch-docs;
              buildInputs = with pkgs; [ mdbook ];
              buildPhase = ''
                mdbook build . -d $out
              '';
            };
            haddock = (import ./nix/combine-haddock.nix) { inherit pkgs lib; } {
              cabalProject = project;
              targetPackages = [
                "plutarch"
                "plutus-core"
                "plutus-tx"
                "plutus-ledger-api"
              ];
              prologue = ''
                = Plutarch Documentation
                Documentation of Plutarch /and/ Documentation of Plutus libraries.
              '';

            };
            # We do have keys for signing hackage set exposed in the repository. Once I figure out how to
            # store secrets in Hercules CI, I'd have to fix this.
            # However, since deployment of hackage sets are fully automatized using Hercules CI,
            # This should remain secure as long as Plutonomicon/Plutarch repository is secure.
            hackage =
              herbage.genHackage
                ./keys
                (import ./nix/hackage.nix { inherit pkgs; });

            combined-docs = pkgs.runCommand "combined-docs"
              { } ''
              mkdir -p $out/haddock
              cp ${self'.packages.haddock}/share/doc/* $out/haddock -r
              cp ${self'.packages.plutarch-docs}/* $out -r
              cp ${self'.packages.hackage}/* $out -r
            '';
          };

          inherit (flake) checks;
        };
    };
}
