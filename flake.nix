{
  description = "Haskell 'network-simple' libraries";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs?rev=21eda9bc80bef824a037582b1e5a43ba74e92daa";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      flake.overlays.default = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = prev.lib.composeExtensions
            (prev.haskell.packageOverrides or (_: _: { })) (hself: hsuper: {
              network-simple = hself.callPackage ./network-simple { };
              network-simple-tls = hself.callPackage ./network-simple-tls { };
              network-simple-ws = hself.callPackage ./network-simple-ws { };
              network-simple-wss = hself.callPackage ./network-simple-wss { };
            });
        };
      };
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
              config.packages.network-simple__ghc8107
              config.packages.network-simple__ghc8107.doc
              config.packages.network-simple__ghc925
              config.packages.network-simple__ghc925.doc
              config.packages.network-simple__ghc943
              config.packages.network-simple__ghc943.doc

              config.packages.network-simple-tls__ghc8107
              config.packages.network-simple-tls__ghc8107.doc
              config.packages.network-simple-tls__ghc925
              config.packages.network-simple-tls__ghc925.doc
              config.packages.network-simple-tls__ghc943
              config.packages.network-simple-tls__ghc943.doc

              config.packages.network-simple-ws__ghc8107
              config.packages.network-simple-ws__ghc8107.doc
              config.packages.network-simple-ws__ghc925
              config.packages.network-simple-ws__ghc925.doc
              config.packages.network-simple-ws__ghc943
              config.packages.network-simple-ws__ghc943.doc

              config.packages.network-simple-wss__ghc8107
              config.packages.network-simple-wss__ghc8107.doc
              config.packages.network-simple-wss__ghc925
              config.packages.network-simple-wss__ghc925.doc
              config.packages.network-simple-wss__ghc943
              config.packages.network-simple-wss__ghc943.doc

              config.devShells.ghc8107
              config.devShells.ghc925
              config.devShells.ghc943
            ];
          };
          network-simple__ghc8107 = pkgs.haskell.packages.ghc8107.network-simple;
          network-simple__ghc925 = pkgs.haskell.packages.ghc925.network-simple;
          network-simple__ghc943 = pkgs.haskell.packages.ghc943.network-simple;

          network-simple-tls__ghc8107 = pkgs.haskell.packages.ghc8107.network-simple-tls;
          network-simple-tls__ghc925 = pkgs.haskell.packages.ghc925.network-simple-tls;
          network-simple-tls__ghc943 = pkgs.haskell.packages.ghc943.network-simple-tls;

          network-simple-ws__ghc8107 = pkgs.haskell.packages.ghc8107.network-simple-ws;
          network-simple-ws__ghc925 = pkgs.haskell.packages.ghc925.network-simple-ws;
          network-simple-ws__ghc943 = pkgs.haskell.packages.ghc943.network-simple-ws;

          network-simple-wss__ghc8107 = pkgs.haskell.packages.ghc8107.network-simple-wss;
          network-simple-wss__ghc925 = pkgs.haskell.packages.ghc925.network-simple-wss;
          network-simple-wss__ghc943 = pkgs.haskell.packages.ghc943.network-simple-wss;
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
          default = config.devShells.ghc943;
          ghc8107 = shellFor pkgs.haskell.packages.ghc8107;
          ghc925 = shellFor pkgs.haskell.packages.ghc925;
          ghc943 = shellFor pkgs.haskell.packages.ghc943;
        };
      };
    };
}
