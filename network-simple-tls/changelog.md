# Version 0.4

* COMPILER ASSISTED BREAKING CHANGE: `ClientSettings` and `ServerSettings` are
  gone.  Instead, `ClientParams` and `ServerParams` from the `tls` package are
  now used throughout.

  The related functions `updateClientParams`, `clientParams`,
  `updateServerParams`, `serverParams` are gone.

  `makeClientSettings` was renamed to `makeClientParams`, which returns
  `ClientParams` and takes `[Credential]` rather than `Credentials`.

  `makeServerSettings` was renamed to `makeServerParams`, which returns
  `ServerParams`.

  `getDefaultClientSettings` was replaced by `newDefaultClientParams`, which adds
  an in-memory `SessionManager` to the `ClientParams`.

* COMPILER ASSISTED BREAKING CHANGE: `Credentials` from `Network.TLS` is not
  re-exported anymore.

* Added `newDefaultServerParams`, which creates a `ServerParams` with an
  in-memory `SessionManager`.

* Re-export `ServerParams`, `ClientParams` from `Network.TLS`.

* Export `credentialLoadX509`, which is the same as
  `Network.TLS.credentialLoadX509` but runs in `MonadIO`.

* Support TLS 1.3, TLS 1.2 and TLS 1.1 by default.

* Bump version dependency on `tls` to `>= 1.5`.

* Add dependency on `tls-session-manager`.


# Version 0.3.2

* Added `sendLazy`.

* Fixed space leak on `recv`. See issue #13.


# Version 0.3.1

* Added SOCKS5 proxy support. See functions `connectOverSOCKS5` and
  `connectTlsOverSOCKS5`.

* Use `safe-exceptions`.


# Version 0.3

* BREAKING CHANGE: Changed type of the following functions:
  `getDefaultClientSettings`, `makeClientSettings`, `updateClientParams`,
  `clientParams`, `makeServerSettings`, `updateServerParams`, `serverParams`.

* BREAKING CHANGE: Only TLS 1.1 and TLS 1.2 are supported by default.

* Server's choice of ciphers are always prefered over client's.

* Server code will mandate strong cipher requirements, client code will be more
  permissive.

* Compatible with `tls-1.4`

* Remove upper bounds for all dependencies except `base`.


# Version 0.2.1

* Ensure that the Socket TLS backend always receive the expected number
  of bytes. This issue showed up as the following exception previously:

      Error_Packet "partial packet: expecting 100 bytes, got: 6"


# Version 0.2.0

* Re-export `Socket`, `SockAddr`, `HostName` and `ServiceName` from
  `Network.Socket` at `Network.Simple.TCP.TLS`.

* Re-export `Context` from `Network.TLS` at `Network.Simple.TCP.TLS`.

* Generalize the `IO` monad by using `MonadIO` and `MonadCatch` (from
  the `exceptions` library).

* Added `makeClientContext`, `makeServerContext` and `useTlsThenClose`.

* Use `Socket` as a TLS backend instead of `Handle`.

* Drop dependency on `monad-random-api` in favour of `monad-random`.

* Dependency bumps.


# Version 0.1.1.0

* Export 'Network.Socket.withSocketsDo' from 'Network.Simple.TCP.TLS'.


# Version 0.1.0.1

* Dependency bumps.


# Version 0.1.0.0

* First release.
