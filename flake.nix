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
            pkgs.zlib
            pkgs.zlib.dev
            pkgs.pkg-config
          ];
          PKG_CONFIG_PATH = "${pkgs.zlib.dev}/lib/pkgconfig";
          shellHook = if pkgs.stdenv.isDarwin then ''
            export DYLD_LIBRARY_PATH=${pkgs.zlib}/lib:$DYLD_LIBRARY_PATH
            export C_INCLUDE_PATH=${pkgs.zlib.dev}/include:$C_INCLUDE_PATH
            export LIBRARY_PATH=${pkgs.zlib}/lib:$LIBRARY_PATH
          '' else ''
            export LD_LIBRARY_PATH=${pkgs.zlib}/lib:$LD_LIBRARY_PATH
          '';
        };
    in {
      devShells.aarch64-darwin.default = mkShell "aarch64-darwin";
      devShells.x86_64-linux.default = mkShell "x86_64-linux";
    };
}
