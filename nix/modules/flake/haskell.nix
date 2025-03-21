{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];
  perSystem = { self', lib, config, pkgs, ... }: {
    # Our only Haskell project. You can have multiple projects, but this template
    # has only one.
    # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
    haskellProjects.default = {
      # To avoid unnecessary rebuilds, we filter projectRoot:
      # https://community.flake.parts/haskell-flake/local#rebuild
      projectRoot = builtins.toString (lib.fileset.toSource {
        inherit root;
        fileset = lib.fileset.unions [
          (root + /src)
          (root + /devden-stat-bot.cabal)
          (root + /LICENSE)
          (root + /README.md)
        ];
      });

      # The base package set (this value is the default)
      basePackages = pkgs.haskell.packages.ghc98;

      # Packages to add on top of `basePackages`
      packages = {
        # Add source or Hackage overrides here
        # (Local packages are added automatically)
        /*
        aeson.source = "1.5.0.0" # Hackage version
        shower.source = inputs.shower; # Flake input
        */

        websockets.source = "0.13.0.0";
      };

      # Add your package overrides here
      settings = {
        devden-stat-bot = {
          stan = true;
          # haddock = false;
        };
        /*
        aeson = {
          check = false;
        };
        */

        calamity.broken = false;
      };

      # Development shell configuration
      devShell = {
        hlsCheck.enable = false;
      };

      # What should haskell-flake add to flake outputs?
      autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
    };

    # Default package & app.
    packages.default = self'.packages.devden-stat-bot;
    apps.default = self'.apps.devden-stat-bot;
  };
}
