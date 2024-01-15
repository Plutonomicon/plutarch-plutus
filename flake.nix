{
  description = "plutarch";

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
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

    CHaP.url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
    CHaP.flake = false;

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";

    emanote.url = "github:srid/emanote";
  };

  outputs = inputs@{ flake-parts, nixpkgs, haskell-nix, iohk-nix, CHaP, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./nix/pre-commit.nix
        ./nix/hercules-ci.nix
        inputs.emanote.flakeModule
      ];
      debug = true;
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      hercules-ci.github-pages.branch = "master";

      perSystem = { config, system, lib, self', ... }:
        let
          pkgs =
            import haskell-nix.inputs.nixpkgs {
              inherit system;
              overlays = [
                haskell-nix.overlay
                iohk-nix.overlays.crypto
              ];
              inherit (haskell-nix) config;
            };
          project = pkgs.haskell-nix.cabalProject' {
            src = ./.;
            compiler-nix-name = "ghc963";
            # NOTE(bladyjoker): Follow https://github.com/input-output-hk/plutus/blob/master/cabal.project
            index-state = "2023-11-26T21:52:49Z";
            inputMap = {
              "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
            };
            shell = {
              withHoogle = true;
              withHaddock = true;
              exactDeps = false;
              # TODO(peter-mlabs): Use `apply-refact` for repo wide refactoring `find -name '*.hs' -not -path './dist-*/*' -exec hlint -j --refactor --refactor-options="--inplace" {} +``
              shellHook = config.pre-commit.installationScript;
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
          emanote = {
            sites.plutarch-docs = {
              layers = [ ./plutarch-docs ];
              layersString = [ "./plutarch-docs" ];
              baseUrl = "/plutarch-plutus/";
            };
          };

          hercules-ci.github-pages.settings.contents = self'.packages.combined-docs;
          packages = flake.packages // {
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
            combined-docs = pkgs.runCommand "combined-docs"
              { } ''
              mkdir -p $out/haddock
              cp ${self'.packages.haddock}/share/doc/* $out/haddock -r
              cp ${self'.packages.plutarch-docs}/* $out -r
            '';
          };

          checks = flake.checks // {
            plutarch-test = pkgs.stdenv.mkDerivation
              {
                name = "plutarch-test";
                src = ./plutarch-test;
                nativeBuildInputs = [ flake.packages."plutarch-test:exe:plutarch-test" ];
                buildPhase = ''
                  plutarch-test
                  touch $out
                '';
              };
          };
        };
    };
}
