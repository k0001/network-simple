{ mkDerivation, base, bytestring, exceptions, network
, safe-exceptions, stdenv, transformers
}:
mkDerivation {
  pname = "network-simple";
  version = "0.4.2";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring exceptions network safe-exceptions transformers
  ];
  homepage = "https://github.com/k0001/network-simple";
  description = "Simple network sockets usage patterns";
  license = stdenv.lib.licenses.bsd3;
}
