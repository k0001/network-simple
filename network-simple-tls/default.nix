{ mkDerivation, base, bytestring, data-default, lib, network
, network-simple, safe-exceptions, tls, tls-session-manager
, transformers, x509, x509-store, x509-system, x509-validation
}:
mkDerivation {
  pname = "network-simple-tls";
  version = "0.4.1";
  src = lib.sources.cleanSource ./.;
  libraryHaskellDepends = [
    base bytestring data-default network network-simple safe-exceptions
    tls tls-session-manager transformers x509 x509-store x509-system
    x509-validation
  ];
  homepage = "https://hackage.haskell.org/package/network-simple-tls";
  description = "Simple interface to TLS secured network sockets";
  license = lib.licenses.bsd3;
}
