# This file is a nixpkgs package set overlay to be used as:
#   import nixpkgs { overlays = [ import ./pkgs-overlay.hs ]; }
self: super:
{
  _here = {
    ghc844 = super.haskell.packages.ghc844.override {
      packageSetConfig = import ./hs-overlay.nix { pkgs = self; };
    };
  };
}
