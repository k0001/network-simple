{ mkDerivation, base, bytestring, crypton-x509, crypton-x509-store
, crypton-x509-system, crypton-x509-validation, data-default, lib
, network, network-simple, safe-exceptions, tls
, tls-session-manager, transformers
}:
mkDerivation {
  pname = "network-simple-tls";
  version = "0.4.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring crypton-x509 crypton-x509-store crypton-x509-system
    crypton-x509-validation data-default network network-simple
    safe-exceptions tls tls-session-manager transformers
  ];
  homepage = "https://hackage.haskell.org/package/network-simple-tls";
  description = "Simple interface to TLS secured network sockets";
  license = lib.licenses.bsd3;
}
