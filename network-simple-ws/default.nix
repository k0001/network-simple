{ mkDerivation, async, base, bytestring, case-insensitive, lib
, network-simple, safe-exceptions, websockets
}:
mkDerivation {
  pname = "network-simple-ws";
  version = "0.2";
  src = ./.;
  libraryHaskellDepends = [
    async base bytestring case-insensitive network-simple
    safe-exceptions websockets
  ];
  homepage = "https://github.com/k0001/network-simple-ws";
  description = "Simple interface to WebSockets";
  license = lib.licenses.bsd3;
}
