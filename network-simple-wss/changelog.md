# Version 0.2

* /COMPILER ASSISTED BREAKING CHANGE:/ `send` and `recv` now use
  lazy `ByteString`s. 

* /COMPILER ASSISTED BREAKING CHANGE:/ `recv`'s close semantics have 
  improved. See its documentation for details.

* Re-exported `close` and `clientConnectionFromStream` from 
  `network-simple-ws`.

* Depend on `network-simple-tls >=0.4`.

* Depend on `network-simple-ws >=0.2`.

* Depend on `websockets >=0.12.6`.


# Version 0.1

* First release.
