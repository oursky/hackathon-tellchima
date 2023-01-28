{
  description = "Tellchima in rust";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    old_nixpkgs.url = "github:nixos/nixpkgs/nixos-20.09";
    rust-overlay.url = "github:oxalica/rust-overlay/stable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, old_nixpkgs, rust-overlay, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
        old_pkgs = import old_nixpkgs {
          inherit system;
        };
        mkShell = pkgs.mkShell.override {
          stdenv = pkgs.stdenvNoCC;
        };
      in
      {
        devShells.default = mkShell {
          packages = [
            pkgs.openssl_1_1
            pkgs.pkg-config
            pkgs.cargo
            pkgs.diesel-cli
          ];

          shellHook = ''
            ldd --version
          '';
        };
      }
    );
}
