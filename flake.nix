{
  description = "plutarch";

  nixConfig = {
    extra-experimental-features = [ "nix-command" "flakes" "ca-derivations" ];
    extra-substituters = [ "https://cache.iog.io" "https://public-plutonomicon.cachix.org" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=" ];
    allow-import-from-derivation = "true";
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix \\[\\e[0;1m\\]plutarch \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
    cores = "1";
    max-jobs = "auto";
    auto-optimise-store = "true";
  };

  inputs.tooling.url = "github:mlabs-haskell/mlabs-tooling.nix?ref=las/work";

  outputs = inputs@{ self, tooling, ... }: tooling.lib.mkFlake { inherit self; }
    {
      imports = [
        (tooling.lib.mkHaskellFlakeModule1 {
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
        })
      ];

      perSystem = { config, pkgs, ... }: {
        checks.plutarch-test = pkgs.runCommand "plutarch-test"
          {
            nativeBuildInputs = [ config.packages."plutarch-test:exe:plutarch-test" ];
          } ''
          plutarch-test
          touch $out
        '';
      };
    };
}
