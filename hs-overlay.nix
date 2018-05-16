# This expression can be used as a Haskell package set `packageSetConfig`:
self: super: {
  network-simple = super.callPackage ./pkg.nix {};
}
