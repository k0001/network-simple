{ mkDerivation, async, base, bytestring, lib, network-simple-tls
, network-simple-ws, safe-exceptions, websockets
}:
mkDerivation {
  pname = "network-simple-wss";
  version = "0.2";
  src = lib.sources.cleanSource ./.;
  libraryHaskellDepends = [
    async base bytestring network-simple-tls network-simple-ws
    safe-exceptions websockets
  ];
  homepage = "https://github.com/k0001/network-simple-wss";
  description = "Simple interface to TLS secured WebSockets";
  license = lib.licenses.bsd3;
}
