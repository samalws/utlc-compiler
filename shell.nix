{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = [ pkgs.rustc (pkgs.buildPackages.ghc.withPackages (p: [p.extra])) ];
}
