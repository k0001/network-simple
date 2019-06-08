{ pkgs }:

let
  hs = pkgs.haskell.lib;
  socks =
    { mkDerivation, base, basement, bytestring, cereal, network, stdenv
    }:
    mkDerivation {
      pname = "socks";
      version = "0.6.0";
      sha256 = "9762fa87aeda7cf98290cb66af91c4ed5bf258b3548b189e9188d0c29f707381";
      libraryHaskellDepends = [
        base basement bytestring cereal network
      ];
      homepage = "http://github.com/vincenthz/hs-socks";
      description = "Socks proxy (ver 5)";
      license = stdenv.lib.licenses.bsd3;
    };
in

# To be used as `packageSetConfig` for a Haskell pacakge set:
self: super:
{
  network-simple = super.callPackage ./pkg.nix {};
  _shell = self.shellFor {
    withHoogle = false; # hoogle dependencies don't compile
    packages = p: [
      p.network-simple
    ];
  };

  #deps
  socks = super.callPackage socks {};
}
