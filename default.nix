{ nixpkgs ? import ./nixpkgs.nix }:
let pkgs = import ./pkgs.nix { inherit nixpkgs; };
in
pkgs.releaseTools.aggregate {
  name = "everything";
  constituents = [
    pkgs._here.ghc844.network-simple
    pkgs._here.ghc844.network-simple.doc
    pkgs._here.ghc844._shell
  ];
}

