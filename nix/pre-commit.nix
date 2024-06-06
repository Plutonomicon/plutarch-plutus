{ inputs, ... }: {
  imports = [
    inputs.pre-commit-hooks.flakeModule
  ];

  perSystem = { config, ... }:
    {
      devShells.dev-pre-commit = config.pre-commit.devShell;

      pre-commit = {
        settings = {
          src = ./.;

          hooks = {
            nixpkgs-fmt.enable = true;
            statix.enable = true;
            deadnix.enable = true;

            cabal-fmt.enable = true;
            fourmolu = {
              enable = true;
              excludes = [ "\.lhs" ];
            };
            ormolu = {
              settings.cabalDefaultExtensions = true;
            };
            hlint.enable = true;

            typos = {
              enable = true;
              excludes = [ "\.golden" "fourmolu.yaml" ];
            };

            yamllint.enable = true;
          };
        };
      };
    };
}
