{ mkDerivation, async, base, bytestring, case-insensitive, lib
, network-simple, safe-exceptions, websockets
}:
mkDerivation {
  pname = "network-simple-ws";
  version = "0.2";
  src = lib.sources.cleanSource ./.;
  libraryHaskellDepends = [
    async base bytestring case-insensitive network-simple
    safe-exceptions websockets
  ];
  homepage = "https://hackage.haskell.org/package/network-simple-ws";
  description = "Simple interface to WebSockets";
  license = lib.licenses.bsd3;
}
