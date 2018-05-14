#! /usr/bin/env nix-shell
#! nix-shell ./shell.nix -i runghc
import Distribution.Simple
main = defaultMain
