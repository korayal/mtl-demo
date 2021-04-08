# { pkgs ? import <nixpkgs> {} }:

# pkgs.haskellPackages.ghcWithPackages (ps: with ps;
#   [
#     cabal-install
#     hpack
#     relude
#     mtl
#     persistent
#     text
#     transformers
#   ])

{ pkgs ? import <nixpkgs> {} }:
let
  ghc = pkgs.haskell.packages.ghc8104.ghcWithPackages (p: with p;
    [
    hpack
    cabal-install

    exceptions
    monad-control
    mtl
    persistent
    persistent-sqlite
    persistent-template
    relude
    snap-core
    snap-server
    text
    transformers
    transformers-base
    unliftio
    unliftio-core
    ]);
in
pkgs.mkShell {
  buildInputs = [ ghc ];
}
