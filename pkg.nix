{ mkDerivation, base, bytestring, network, network-bsd
, safe-exceptions, socks, stdenv, transformers
}:
mkDerivation {
  pname = "network-simple";
  version = "0.4.5";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring network network-bsd safe-exceptions socks
    transformers
  ];
  homepage = "https://github.com/k0001/network-simple";
  description = "Simple network sockets usage patterns";
  license = stdenv.lib.licenses.bsd3;
}
