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

  inputs.tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";
  inputs.nixpkgs.follows = "tooling/nixpkgs";
  inputs.hercules-ci-effects = {
    url = "github:hercules-ci/hercules-ci-effects";
    inputs.nixpkgs.follows = "tooling/nixpkgs";
    inputs.flake-parts.follows = "tooling/flake-parts";
  };

  outputs = inputs@{ self, tooling, hercules-ci-effects, nixpkgs, ... }: tooling.lib.mkFlake { inherit self; }
    ({ withSystem, ... }: {
      imports = [
        (tooling.lib.mkHaskellFlakeModule1 {
          docsPath = ./plutarch-docs;
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

      systems = [ "x86_64-linux" "aarch64-linux" ];

      perSystem = { config, pkgs, ... }: {
        checks.plutarch-test = pkgs.runCommand "plutarch-test"
          {
            nativeBuildInputs = [ config.packages."plutarch-test:exe:plutarch-test" ];
          } ''
          plutarch-test
          touch $out
        '';
      };

      # hercules-ci-effects
      flake.effects =
        let
          ciSystem = "x86_64-linux";
          pkgs = nixpkgs.legacyPackages.${ciSystem};
          hci-effects = hercules-ci-effects.lib.withPkgs pkgs;
        in
        { branch, ... }:
        # TODO: add an effect to keep staging up to date, too
        hci-effects.runIf (branch == "master") (hci-effects.mkEffect {
          src = self;
          buildInputs = [ pkgs.openssh pkgs.git ];
          secretsMap.token = { type = "GitToken"; };
          EMAIL = "hercules-ci[bot]@users.noreply.github.com";
          GIT_AUTHOR_NAME = "Hercules CI Effects";
          GIT_COMMITTER_NAME = "Hercules CI Effects";
          PAGER = "cat";
          userSetupScript =
            ''
              set -x
              echo "https://git:$(readSecretString token .token)@github.com/Plutonomicon/plutarch-plutus" >~/.git-credentials
              git config --global credential.helper store
            '';
          effectScript =
            ''
              cp -r --no-preserve=mode ${self.packages.${ciSystem}.docs} ./gh-pages && cd gh-pages
              echo "plutarch-plutus" > CNAME
              git init -b gh-pages
              git remote add origin https://github.com/Plutonomicon/plutarch-plutus
              git add .
              git commit -m "Deploy to gh-pages"
              git push -vvvvv -f origin gh-pages:gh-pages
            '';
        });
    });
}
