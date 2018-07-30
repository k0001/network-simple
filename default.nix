{ nixpkgs ? import ./nixpkgs.nix }:

let
pkgs = import nixpkgs {};
ghc843 = pkgs.haskell.packages.ghc843.override {
  packageSetConfig = import ./hs-overlay.nix { inherit pkgs; };
};

in { inherit (ghc843) network-simple; }
