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

  # FIXME: we need to think about what's going on here, both hci-effects and tooling
  #        implement the same argument, I don't know how to solve this atm
  inputs.tooling.url = "github:mlabs-haskell/mlabs-tooling.nix/mangoiv/fix-herculesCI-arg";


  outputs = inputs@{ tooling, ... }: tooling.lib.mkFlake { inherit inputs; }
    ({ withSystem, ... }: {
      imports = [
        tooling.lib.hercules-flakeModule
        (tooling.lib.mkHaskellFlakeModule1 {
          docsPath = ./plutarch-docs;
          baseUrl = "/plutarch-plutus/";
          toHaddock = [ "plutarch" "plutus-core" "plutus-tx" "plutus-ledger-api" ];
          project.src = ./.;
          project.modules = [
            ({ config, pkgs, hsPkgs, ... }: {
              packages = {
                # Workaround missing support for build-tools:
                # https://github.com/input-output-hk/haskell.nix/issues/231
                plutarch-docs.components.exes.plutarch-docs.build-tools = [
                  config.hsPkgs.markdown-unlit
                ];
                plutarch-test.components.exes.plutarch-test.build-tools = [
                  config.hsPkgs.hspec-discover
                ];
              };
            })
          ];
        })
      ];

      systems =
        if builtins.hasAttr "currentSystem" builtins
        then [ builtins.currentSystem ]
        else [ "x86_64-linux" "aarch64-linux" ];

      hercules-ci.github-pages.branch = "master";
      herculesCI.ciSystems = [ "x86_64-linux" ];

      perSystem = { config, pkgs, self', system, ... }: {
        packages.combined-docs = pkgs.runCommand "combined-docs" { buildInputs = with config.packages; [ docs haddock ]; } ''
          mkdir -p $out/share/doc
          cp -r ${config.packages.docs}/* $out
          cp -r ${config.packages.haddock}/share/doc/* $out/share/doc
        '';
        hercules-ci.github-pages.settings.contents = config.packages.combined-docs;

        checks.plutarch-test = pkgs.runCommand "plutarch-test"
          {
            nativeBuildInputs = [ config.packages."plutarch-test:exe:plutarch-test" ];
          } ''
          plutarch-test
          touch $out
        '';
      };
    });
}
