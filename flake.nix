{
  description = "plutarch";

  inputs.tooling.url = "github:mlabs-haskell/mlabs-tooling.nix?ref=las/work";

  outputs = inputs@{ self, tooling, ... }: tooling.mkHaskellFlake1 {
      docsPath = ./docs;
      toHaddock = [ "plutarch" "plutus-core" "plutus-tx" "plutus-ledger-api" ];
      project.src = ./.;
      project.modules = [
        ({ config, pkgs, hsPkgs, ... }: {
          packages = {
            # Workaround missing support for build-tools:
            # https://github.com/input-output-hk/haskell.nix/issues/231
            plutarch-test.components.exes.plutarch-test.build-tools = [
              config.hsPkgs.hspec-discover
            ];
          };
        })
      ];
    };
}
