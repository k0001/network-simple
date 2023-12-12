{
  description = "Haskell 'network-simple' libraries";

  inputs = {
    flakety.url = "github:k0001/flakety";
    nixpkgs.follows = "flakety/nixpkgs";
    flake-parts.follows = "flakety/flake-parts";
  };

  outputs = inputs@{ ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      flake.overlays.default = inputs.nixpkgs.lib.composeManyExtensions [
        inputs.flakety.overlays.default
        (final: prev:
          let
            hsLib = prev.haskell.lib;
            hsClean = drv:
              hsLib.overrideCabal drv
              (old: { src = prev.lib.sources.cleanSource old.src; });
          in {
            haskell = prev.haskell // {
              packageOverrides = prev.lib.composeExtensions
                (prev.haskell.packageOverrides or (_: _: { })) (hself: hsuper: {
                  network-simple =
                    hsClean (hself.callPackage ./network-simple { });
                  network-simple-tls =
                    hsClean (hself.callPackage ./network-simple-tls { });
                  network-simple-ws =
                    hsClean (hself.callPackage ./network-simple-ws { });
                  network-simple-wss =
                    hsClean (hself.callPackage ./network-simple-wss { });
                });
            };
          })
      ];
      systems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      perSystem = { config, system, pkgs, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.self.overlays.default ];
        };
        packages = {
          default = pkgs.releaseTools.aggregate {
            name = "every output from this flake";
            constituents = [
              config.packages.network-simple__ghc98
              config.packages.network-simple__ghc98.doc
              config.packages.network-simple__ghc96
              config.packages.network-simple__ghc96.doc

              config.packages.network-simple-tls__ghc98
              config.packages.network-simple-tls__ghc98.doc
              config.packages.network-simple-tls__ghc96
              config.packages.network-simple-tls__ghc96.doc

              config.packages.network-simple-ws__ghc98
              config.packages.network-simple-ws__ghc98.doc
              config.packages.network-simple-ws__ghc96
              config.packages.network-simple-ws__ghc96.doc

              config.packages.network-simple-wss__ghc98
              config.packages.network-simple-wss__ghc98.doc
              config.packages.network-simple-wss__ghc96
              config.packages.network-simple-wss__ghc96.doc

              config.devShells.ghc98
              config.devShells.ghc96
            ];
          };
          network-simple__ghc98 = pkgs.haskell.packages.ghc98.network-simple;
          network-simple__ghc96 = pkgs.haskell.packages.ghc96.network-simple;

          network-simple-tls__ghc98 =
            pkgs.haskell.packages.ghc98.network-simple-tls;
          network-simple-tls__ghc96 =
            pkgs.haskell.packages.ghc96.network-simple-tls;

          network-simple-ws__ghc98 =
            pkgs.haskell.packages.ghc98.network-simple-ws;
          network-simple-ws__ghc96 =
            pkgs.haskell.packages.ghc96.network-simple-ws;

          network-simple-wss__ghc98 =
            pkgs.haskell.packages.ghc98.network-simple-wss;
          network-simple-wss__ghc96 =
            pkgs.haskell.packages.ghc96.network-simple-wss;
        };
        devShells = let
          shellFor = hpkgs:
            hpkgs.shellFor {
              packages = p: [
                p.network-simple
                p.network-simple-tls
                p.network-simple-ws
                p.network-simple-wss
              ];
              withHoogle = true;
              nativeBuildInputs = [ pkgs.cabal-install pkgs.cabal2nix ];
            };
        in {
          default = config.devShells.ghc98;
          ghc98 = shellFor pkgs.haskell.packages.ghc98;
          ghc96 = shellFor pkgs.haskell.packages.ghc96;
        };
      };
    };
}
