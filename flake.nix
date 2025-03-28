{
  description = "Dev flake for haskell";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };
  outputs = {nixpkgs, ...}: let
    supportedSystems = ["x86_64-linux" "aarch64-darwin"];
    forEachSupportedSystem = f:
      nixpkgs.lib.genAttrs supportedSystems (system:
        f {
          pkgs = import nixpkgs {inherit system;};
        });
  in {
    devShells = forEachSupportedSystem ({pkgs}: {
      default = pkgs.mkShell {
        buildInputs = [pkgs.ghc pkgs.haskell-language-server];
      };
    });
  };
}
