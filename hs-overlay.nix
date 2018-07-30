{ pkgs }:

let
hs = pkgs.haskell.lib;

in
# This expression can be used as a Haskell package set `packageSetConfig`:
self: super: {
  safe-exceptions = hs.doJailbreak super.safe-exceptions;
  network-simple = super.callPackage ./pkg.nix {};
}
