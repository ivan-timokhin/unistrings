{ nixpkgs ? import <nixpkgs-unstable> {}, compiler ? "ghc865" }:
with nixpkgs;
haskell.packages.${compiler}.developPackage {
  root = ./.;
}
