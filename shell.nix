# This file, intended to be used with nix-shell, puts you in an
# environment where all of the dependencies necessary to work on the
# various Haskell projects in this repo are available.
{ nixpkgs ? import ./nixpkgs.nix }:
let pkgs = import ./pkgs.nix { inherit nixpkgs; };
in pkgs._here.ghc844._shell
