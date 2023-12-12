{ mkDerivation, base, bytestring, lib, network, network-bsd
, safe-exceptions, socks, transformers
}:
mkDerivation {
  pname = "network-simple";
  version = "0.4.5";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring network network-bsd safe-exceptions socks
    transformers
  ];
  homepage = "https://hackage.haskell.org/package/network-simple";
  description = "Simple network sockets usage patterns";
  license = lib.licenses.bsd3;
}
