{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = [ pkgs.rustc pkgs.cargo (pkgs.buildPackages.ghc.withPackages (p: [p.transformers-either])) ];
}
