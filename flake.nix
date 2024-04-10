{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    servant-effectful = {
      url = "github:Diamondy4/servant-effectful";
      flake = false;
    };
    gogol.url = "github:brendanhay/gogol";
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];

      perSystem = {
        self',
        pkgs,
        lib,
        config,
        system,
        ...
      }: {
        _module.args.pkgs = import self.inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.gogol.overlays.gogol ];
        };
        haskellProjects.default = {
          packages = {
            servant-effectful.source = inputs.servant-effectful;
          };
          settings.servant-effectful.check = false;
          devShell.tools = hp: {
            inherit (hp) cabal-fmt dhall-lsp-server;
          };
        };
      };
    };
}
