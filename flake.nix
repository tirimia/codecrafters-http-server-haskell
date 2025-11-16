{
  description = "Dev flake for haskell";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/a2eb207f45e4a14a1e3019d9e3863d1e208e2295";
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
        packages = [pkgs.clang_21 (pkgs.haskellPackages.ghcWithPackages (hp: with hp;[ haskell-language-server stack ghc]))];
      };
    });
  };
}
