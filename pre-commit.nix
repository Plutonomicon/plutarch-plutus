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
          settings = {
            ormolu.cabalDefaultExtensions = true;
          };

          hooks = {
            nixpkgs-fmt.enable = true;
            statix.enable = true;
            deadnix.enable = true;

            cabal-fmt.enable = true;
            fourmolu = {
              enable = true;
              excludes = [ "\.lhs" ];
            };
            hlint.enable = true;

            typos = {
              enable = true;
              excludes = [ "\.golden" ];
            };

            yamllint.enable = true;
          };
        };
      };
    };
}
