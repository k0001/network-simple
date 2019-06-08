# This file exports a nixpkgs package set including all of the stuff
# from our repo. See pkgs-overlay for details.
{ nixpkgs ? import ./nixpkgs.nix }:
import nixpkgs { overlays = [ (import ./pkgs-overlay.nix) ]; }
