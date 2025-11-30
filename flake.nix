{
  description = "HTTP Server Haskell Dev Environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";

  outputs = { self, nixpkgs }:
    let
      mkShell = system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          hPkgs = pkgs.haskell.packages.ghc98;
        in pkgs.mkShell {
          buildInputs = [
            hPkgs.ghc
            pkgs.stack
            hPkgs.haskell-language-server
            hPkgs.fourmolu
            pkgs.vegeta
          ];
        };
    in {
      devShells.aarch64-darwin.default = mkShell "aarch64-darwin";
      devShells.x86_64-linux.default = mkShell "x86_64-linux";
    };
}
