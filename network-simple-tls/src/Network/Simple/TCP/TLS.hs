{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module exports simple tools for establishing TLS-secured TCP
-- connections, relevant to both the client side and server side of the
-- connection.
--
-- This module re-exports some functions from the "Network.Simple.TCP" module in
-- the [network-simple](https://hackage.haskell.org/package/network-simple)
-- package. Consider using that module directly if you need a similar API
-- without TLS support.
--
-- This module uses 'MonadIO' and 'E.MonadMask' extensively so that you can
-- reuse these functions in monads other than 'IO'. However, if you don't care
-- about any of that, just pretend you are using the 'IO' monad all the time and
-- everything will work as expected.

module Network.Simple.TCP.TLS (
  -- * Server side
    serve
  -- ** Listening
  , S.listen
  -- ** Accepting
  , accept
  , acceptFork
  -- ** Server TLS Settings
  , newDefaultServerParams
  , makeServerParams

  -- * Client side
  , connect
  , connectOverSOCKS5
  -- ** Client TLS Settings
  , newDefaultClientParams
  , makeClientParams

  -- * Utils
  , recv
  , send
  , sendLazy

  -- * Low level support
  , useTls
  , useTlsThenClose
  , useTlsThenCloseFork
  , connectTls
  , connectTlsOverSOCKS5
  , acceptTls
  , makeClientContext
  , makeServerContext

  -- * Re-exports
  -- $reexports
  , NS.withSocketsDo
  , S.HostPreference(..)
  , NS.HostName
  , NS.ServiceName
    -- | A service port like @"80"@ or its name @"www"@.
  , NS.Socket
  , NS.SockAddr
  , T.Context
  , T.ClientParams
    -- | Please refer to the "Network.TLS" module for more documentation on
    -- 'T.ClientParams`.
    --
    -- There's plenty to be changed, but the documentation for
    -- 'T.ClientParams' is not rendered inside "Network.Simple.TCP.TLS" module.
  , T.ServerParams
    -- | Please refer to the "Network.TLS" module for more documentation on
    -- 'T.ServerParams`.
    --
    -- There's plenty to be changed, but the documentation for
    -- 'T.ServerParams' is not rendered inside "Network.Simple.TCP.TLS" module.
  , T.Credential
  , credentialLoadX509
  ) where


import           Control.Concurrent (ThreadId, forkFinally)
import qualified Control.Exception.Safe as E
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Default (def)
import           Data.List (intersect)
import           Data.Maybe (isJust, listToMaybe)
import qualified Data.X509 as X
import qualified Data.X509.CertificateStore as X
import qualified Data.X509.Validation as X
import           Foreign.C.Error (Errno(Errno), ePIPE)
import qualified GHC.IO.Exception as Eg
import qualified Network.Simple.TCP as S
import qualified Network.Socket as NS
import qualified Network.TLS as T
import qualified Network.TLS.SessionManager as TSM
import qualified Network.TLS.Extra as TE
import           System.X509 (getSystemCertificateStore)

--------------------------------------------------------------------------------


-- $reexports
--
-- For your convenience, this module module also re-exports the following types
-- from other modules:
--
-- [From "Network.Socket"] 'NS.HostName', 'NS.ServiceName', 'NS.Socket',
--   'NS.SockAddr', 'NS.withSocketsDo'.
--
-- [From "Network.Simple.TCP"]
--   @'S.HostPreference'('S.Host','S.HostAny','S.HostIPv4','S.HostIPv6')@.
--
-- [From "Network.TLS"] 'T.Context', 'T.Credential', 'T.ServerParams',
--   'T.ClientParams', 'credentialLoadX509'.

--------------------------------------------------------------------------------
-- Client side TLS settings

-- | Obtain new default 'T.ClientParams' for a particular 'X.ServiceID'.
--
-- * No client credentials sumbitted to the server.
--
-- * Use system-wide CA certificate store.
--
-- * Use an in-memory TLS session manager from the
-- [tls-session-manager](https://hackage.haskell.org/package/tls-session-manager)
-- package.
--
-- * Everything else as proposed by 'makeClientParams'.
newDefaultClientParams
  :: MonadIO m
  => X.ServiceID
  -- ^
  -- @
  -- 'X.ServiceID' ~ ('S.HostName', 'B.ByteString')
  -- @
  --
  -- Identification of the connection consisting of the fully qualified host
  -- name for the server (e.g. www.example.com) and an optional suffix.
  --
  -- It is important that the hostname part is properly filled for security
  -- reasons, as it allow to properly associate the remote side with the given
  -- certificate during a handshake.
  --
  -- The suffix is used to identity a certificate per service on a specific
  -- host. For example, a same host might have different certificates on
  -- differents ports (443 and 995). For TCP connections, it's recommended
  -- to use: @:port@, or @:service@ for the blob (e.g., \@":443"@, @\":https"@).
  -> m T.ClientParams
newDefaultClientParams sid = liftIO $ do
  cs <- getSystemCertificateStore
  sm <- TSM.newSessionManager TSM.defaultConfig
  let cp0 = makeClientParams sid [] cs
  pure $ cp0
    { T.clientShared = (T.clientShared cp0)
        { T.sharedSessionManager = sm }
    }

-- | Make defaults 'T.ClientParams'.
--
-- * Certificate chain validation is done by 'X.validateDefault' from the
-- "Data.X509.Validation" module.
--
-- * The Server Name Indication (SNI) TLS extension is enabled.
--
-- * The supported cipher suites are those enumerated by 'TE.ciphersuite_default',
-- in decreasing order of preference.
--
-- * Secure renegotiation is enabled.
--
-- * Only the __TLS 1.1__, __TLS 1.2__ and __TLS 1.3__ protocols are supported by default.
--
-- If you are unsatisfied with any of these settings, please
-- please refer to the "Network.TLS" module for more documentation on
-- 'T.ClientParams`.
makeClientParams
  :: X.ServiceID
  -- ^
  -- @
  -- 'X.ServiceID' ~ ('S.HostName', 'B.ByteString')
  -- @
  --
  -- Identification of the connection consisting of the fully qualified host
  -- name for the server (e.g. www.example.com) and an optional suffix.
  --
  -- It is important that the hostname part is properly filled for security
  -- reasons, as it allow to properly associate the remote side with the given
  -- certificate during a handshake.
  --
  -- The suffix is used to identity a certificate per service on a specific
  -- host. For example, a same host might have different certificates on
  -- differents ports (443 and 995). For TCP connections, it's recommended
  -- to use: @:port@, or @:service@ for the blob (e.g., \@":443"@, @\":https"@).
  -> [T.Credential]
  -- ^ Credentials to provide to the server if requested. Only credentials
  -- matching the server's 'X.DistinguishedName' will be submitted.
  --
  -- Can be loaded with 'credentialLoadX509' or similar functions.
  -> X.CertificateStore
  -- ^ CAs used to verify the server certificate.
  --
  -- Use 'getSystemCertificateStore' to obtain the operating system's defaults.
  -> T.ClientParams
makeClientParams (hn, sp) creds cStore =
    (T.defaultParamsClient hn sp)
      { T.clientUseServerNameIndication = True
      , T.clientSupported = def
        { T.supportedVersions = [T.TLS13, T.TLS12, T.TLS11]
        , T.supportedCiphers = TE.ciphersuite_default
        , T.supportedSecureRenegotiation = True
        , T.supportedClientInitiatedRenegotiation = True }
      , T.clientShared = def { T.sharedCAStore = cStore }
      , T.clientHooks = def
        { T.onServerCertificate = X.validateDefault
        , T.onCertificateRequest = pure . findCredential }
      }
  where
    -- | Find the first Credential that matches the given requirements.
    -- Currently, the only requirement considered is the subject DN.
    findCredential
      :: ([T.CertificateType],
          Maybe [T.HashAndSignatureAlgorithm],
          [X.DistinguishedName])
      -> Maybe (X.CertificateChain, X.PrivKey)
    findCredential (_, _, dns) = listToMaybe (filter isSubject creds)
      where
        isSubject (X.CertificateChain cc, _) =
          any (\c -> (X.certSubjectDN . X.getCertificate) c `elem` dns) cc

--------------------------------------------------------------------------------
-- Server side TLS settings

-- | Make default 'T.ServerParams'.
--
-- * The supported cipher suites are those enumerated by 'TE.ciphersuite_strong',
-- in decreasing order of preference. The cipher suite preferred by the server
-- is used.
--
-- * Secure renegotiation initiated by the server is enabled, but renegotiation
-- initiated by the client is disabled.
--
-- * Only the __TLS 1.1__, __TLS 1.2__ and __TLS 1.3__ protocols are supported by default.
--
-- If you are unsatisfied with any of these settings, please
-- please refer to the "Network.TLS" module for more documentation on
-- 'T.ServerParams`.
makeServerParams
  :: T.Credential
  -- ^ Server credential.
  --
  -- Can be loaded with 'credentialLoadX509' or similar functions.
  -> Maybe X.CertificateStore
  -- ^ CAs used to verify the client certificate.
  --
  -- If specified, then a valid client certificate will be expected during
  -- handshake.
  --
  -- Use 'getSystemCertificateStore' to obtain the operating system's defaults.
  -> T.ServerParams
makeServerParams cred ycStore = def
      { T.serverWantClientCert = isJust ycStore
      , T.serverShared = def
        { T.sharedCredentials = T.Credentials [cred] }
      , T.serverCACertificates = []
      , T.serverSupported = def
        { T.supportedVersions = [T.TLS13, T.TLS12, T.TLS11]
        , T.supportedCiphers = TE.ciphersuite_strong
        , T.supportedSession = True
        , T.supportedSecureRenegotiation = True
        , T.supportedClientInitiatedRenegotiation = False }
      , T.serverHooks = def
        { T.onClientCertificate = clientCertsCheck
        , T.onCipherChoosing = chooseCipher }
      }
  where
    clientCertsCheck :: X.CertificateChain -> IO T.CertificateUsage
    clientCertsCheck certs = case ycStore of
      Nothing -> return T.CertificateUsageAccept
      Just cs -> do
        let checks = X.defaultChecks { X.checkFQHN = False }
        es <- X.validate X.HashSHA256 X.defaultHooks checks cs def ("","") certs
        case es of
          [] -> pure T.CertificateUsageAccept
          errs' -> pure (T.CertificateUsageReject (T.CertificateRejectOther
                            ("Unacceptable client cert: " ++ show errs')))
    -- Ciphers prefered by the server take precedence.
    chooseCipher :: T.Version -> [T.Cipher] -> T.Cipher
    chooseCipher _ cCiphs = head (intersect TE.ciphersuite_strong cCiphs)

-- | Obtain new default 'T.ServerParams' for a particular server 'T.Credential'.
--
-- * Don't require credentials from clients.
--
-- * Use an in-memory TLS session manager from the
-- [tls-session-manager](https://hackage.haskell.org/package/tls-session-manager)
-- package.
--
-- * Everything else as proposed by 'makeServerParams'.
newDefaultServerParams
  :: MonadIO m
  => T.Credential
  -- ^ Server credential.
  --
  -- Can be loaded with 'credentialLoadX509' or similar functions.
  -> m T.ServerParams
newDefaultServerParams cred = liftIO $ do
  sm <- TSM.newSessionManager TSM.defaultConfig
  let sp0 = makeServerParams cred Nothing
  pure $ sp0
    { T.serverShared = (T.serverShared sp0)
        { T.sharedSessionManager = sm }
    }

--------------------------------------------------------------------------------

-- | Start a TLS-secured TCP server that accepts incoming connections and
-- handles each of them concurrently, in different threads.
--
-- Any acquired network resources are properly closed and discarded when done or
-- in case of exceptions. This function binds a listening socket, accepts an
-- incoming connection, performs a TLS handshake and then safely closes the
-- connection when done or in case of exceptions. You don't need to perform any
-- of those steps manually.
serve
  :: MonadIO m
  => T.ServerParams       -- ^TLS settings.
  -> S.HostPreference     -- ^Preferred host to bind.
  -> S.ServiceName          -- ^Service port to bind.
  -> ((T.Context, S.SockAddr) -> IO ())
                          -- ^Computation to run in a different thread
                          -- once an incomming connection is accepted and a
                          -- TLS-secured communication is established. Takes the
                          -- TLS connection context and remote end address.
  -> m ()
serve ss hp port k = liftIO $ do
    S.listen hp port $ \(lsock,_) -> do
      forever $ acceptFork ss lsock k

--------------------------------------------------------------------------------

-- | Accepts a single incomming TLS-secured TCP connection and use it.
--
-- A TLS handshake is performed immediately after establishing the TCP
-- connection and the TLS and TCP connections are properly closed when done or
-- in case of exceptions. If you need to manage the lifetime of the connection
-- resources yourself, then use 'acceptTls' instead.
accept
  :: (MonadIO m, E.MonadMask m)
  => T.ServerParams       -- ^TLS settings.
  -> S.Socket               -- ^Listening and bound socket.
  -> ((T.Context, S.SockAddr) -> m r)
                          -- ^Computation to run in a different thread
                          -- once an incomming connection is accepted and a
                          -- TLS-secured communication is established. Takes the
                          -- TLS connection context and remote end address.
  -> m r
accept ss lsock k = E.bracket (acceptTls ss lsock)
                              (liftIO . T.contextClose . fst)
                              (useTls k)

-- | Like 'accept', except it uses a different thread to performs the TLS
-- handshake and run the given computation.
acceptFork
  :: MonadIO m
  => T.ServerParams       -- ^TLS settings.
  -> S.Socket               -- ^Listening and bound socket.
  -> ((T.Context, S.SockAddr) -> IO ())
                          -- ^Computation to run in a different thread
                          -- once an incomming connection is accepted and a
                          -- TLS-secured communication is established. Takes the
                          -- TLS connection context and remote end address.
  -> m ThreadId
acceptFork ss lsock k = liftIO $ do
    E.bracketOnError (acceptTls ss lsock)
                     (T.contextClose . fst)
                     (useTlsThenCloseFork k)

--------------------------------------------------------------------------------

-- | Connect to a TLS-secured TCP server and use the connection
--
-- A TLS handshake is performed immediately after establishing the TCP
-- connection and the TLS and TCP connections are properly closed when done or
-- in case of exceptions. If you need to manage the lifetime of the connection
-- resources yourself, then use 'connectTls' instead.
connect
  :: (MonadIO m, E.MonadMask m)
  => T.ClientParams       -- ^ TLS settings.
  -> S.HostName             -- ^ Server hostname.
  -> S.ServiceName          -- ^ Destination server service port name or number.
  -> ((T.Context, S.SockAddr) -> m r)
  -- ^ Computation to run after establishing TLS-secured TCP connection to the
  -- remote server. Takes the TLS connection context and remote end address.
  -> m r
connect cs host port k = E.bracket (connectTls cs host port)
                                   (liftIO . T.contextClose . fst)
                                   (useTls k)

-- | Like 'connect', but connects to the destination server over a SOCKS5 proxy.
connectOverSOCKS5
  :: (MonadIO m, E.MonadMask m)
  => S.HostName        -- ^ SOCKS5 proxy server hostname or IP address.
  -> S.ServiceName     -- ^ SOCKS5 proxy server service port name or number.
  -> T.ClientParams  -- ^ TLS settings.
  -> S.HostName
  -- ^ Destination server hostname or IP address. We connect to this host
  -- /through/ the SOCKS5 proxy specified in the previous arguments.
  --
  -- Note that if hostname resolution on this 'S.HostName' is necessary, it
  -- will happen on the proxy side for security reasons, not locally.
  -> S.ServiceName -- ^ Destination server service port name or number.
  -> ((T.Context, S.SockAddr, S.SockAddr) -> m r)
  -- ^ Computation to run after establishing TLS-secured TCP connection to the
  -- remote server. Takes the TLS connection that can be used to interact with
  -- the destination server, as well as the address of the SOCKS5 server and the
  -- address of the destination server, in that order.
  -> m r
connectOverSOCKS5 phn psn cs dhn dsn k = do
  E.bracket (connectTlsOverSOCKS5 phn psn cs dhn dsn)
            (\(ctx, _, _) -> liftIO (T.contextClose ctx))
            (\(ctx, paddr, daddr) ->
                useTls (\_ -> k (ctx, paddr, daddr))
                       (ctx, paddr))

--------------------------------------------------------------------------------

-- | Estalbishes a TCP connection to a remote server and returns a TLS
-- 'T.Context' configured on top of it using the given 'T.ClientParams'.
-- The remote end address is also returned.
--
-- Prefer to use 'connect' if you will be using the obtained 'T.Context' within a
-- limited scope.
--
-- You need to perform a TLS handshake on the resulting 'T.Context' before using
-- it for communication purposes, and gracefully close the TLS and TCP
-- connections afterwards using. The 'useTls', 'useTlsThenClose' and
-- 'useTlsThenCloseFork' can help you with that.
connectTls
  :: MonadIO m
  => T.ClientParams       -- ^ TLS settings.
  -> S.HostName             -- ^ Server hostname.
  -> S.ServiceName          -- ^ Server service name or port number.
  -> m (T.Context, S.SockAddr)
connectTls cs host port = liftIO $ do
    E.bracketOnError
        (S.connectSock host port)
        (S.closeSock . fst)
        (\(sock, addr) -> do
             ctx <- makeClientContext cs sock
             return (ctx, addr))

-- | Like 'connectTls', but connects to the destination server over a SOCKS5
-- proxy.
connectTlsOverSOCKS5
  :: MonadIO m
  => S.HostName        -- ^ SOCKS5 proxy server hostname or IP address.
  -> S.ServiceName     -- ^ SOCKS5 proxy server service port name or number.
  -> T.ClientParams  -- ^ TLS settings.
  -> S.HostName
  -- ^ Destination server hostname or IP address. We connect to this host
  -- /through/ the SOCKS5 proxy specified in the previous arguments.
  --
  -- Note that if hostname resolution on this 'S.HostName' is necessary, it
  -- will happen on the proxy side for security reasons, not locally.
  -> S.ServiceName -- ^ Destination server service port name or number.
  -> m (T.Context, S.SockAddr, S.SockAddr)
  -- ^ Returns the 'T.Context' that can be used to interact with the destination
  -- server, as well as the address of the SOCKS5 server and the address of the
  -- destination server, in that order.
connectTlsOverSOCKS5 phn psn cs dhn dsn = liftIO $ do
  E.bracketOnError
     (S.connectSock phn psn)
     (S.closeSock . fst)
     (\(psock, paddr) -> do
          daddr <- S.connectSockSOCKS5 psock dhn dsn
          ctx <- makeClientContext cs psock
          return (ctx, paddr, daddr))

-- | Make a client-side TLS 'T.Context' for the given settings, on top of the
-- given TCP `S.Socket` connected to the remote end.
makeClientContext :: MonadIO m => T.ClientParams -> S.Socket -> m T.Context
makeClientContext params sock = liftIO $ T.contextNew sock params

--------------------------------------------------------------------------------

-- | Accepts an incoming TCP connection and returns a TLS 'T.Context' configured
-- on top of it using the given 'T.ServerParams'. The remote end address is also
-- returned.
--
-- Prefer to use 'accept' if you will be using the obtained 'T.Context' within a
-- limited scope.
--
-- You need to perform a TLS handshake on the resulting 'T.Context' before using
-- it for communication purposes, and gracefully close the TLS and TCP
-- connections afterwards using. The 'useTls', 'useTlsThenClose' and
-- 'useTlsThenCloseFork' can help you with that.
acceptTls
  :: MonadIO m
  => T.ServerParams   -- ^TLS settings.
  -> S.Socket           -- ^Listening and bound socket.
  -> m (T.Context, S.SockAddr)
acceptTls sp lsock = liftIO $ do
    E.bracketOnError
        (NS.accept lsock)
        (S.closeSock . fst)
        (\(sock, addr) -> do
             ctx <- makeServerContext sp sock
             return (ctx, addr))

-- | Make a server-side TLS 'T.Context' for the given settings, on top of the
-- given TCP `S.Socket` connected to the remote end.
makeServerContext :: MonadIO m => T.ServerParams -> S.Socket -> m T.Context
makeServerContext params sock = liftIO $ T.contextNew sock params

--------------------------------------------------------------------------------

-- | Perform a TLS handshake on the given 'T.Context', then perform the
-- given action and at last gracefully close the TLS session using `T.bye`.
--
-- This function does not close the underlying TCP connection when done.
-- Prefer to use `useTlsThenClose` or `useTlsThenCloseFork` if you need that
-- behavior. Otherwise, you must call `T.contextClose` yourself at some point.
useTls
  :: (MonadIO m, E.MonadMask m)
  => ((T.Context, S.SockAddr) -> m a)
  -> ((T.Context, S.SockAddr) -> m a)
useTls k conn@(ctx,_) = E.bracket_ (T.handshake ctx)
                                   (liftIO $ silentBye ctx)
                                   (k conn)

-- | Like 'useTls', except it also fully closes the TCP connection when done.
useTlsThenClose
  :: (MonadIO m, E.MonadMask m)
  => ((T.Context, S.SockAddr) -> m a)
  -> ((T.Context, S.SockAddr) -> m a)
useTlsThenClose k conn@(ctx,_) = do
    useTls k conn `E.finally` liftIO (T.contextClose ctx)

-- | Similar to 'useTlsThenClose', except it performs the all the IO actions
-- in a new  thread.
--
-- Use this instead of forking `useTlsThenClose` yourself, as that won't give
-- the right behavior.
useTlsThenCloseFork
  :: MonadIO m
  => ((T.Context, S.SockAddr) -> IO ())
  -> ((T.Context, S.SockAddr) -> m ThreadId)
useTlsThenCloseFork k conn@(ctx,_) = liftIO $ do
    forkFinally (E.bracket_ (T.handshake ctx) (silentBye ctx) (k conn))
                (\eu -> T.contextClose ctx >> either E.throwIO return eu)

--------------------------------------------------------------------------------
-- Utils

-- | Receives decrypted bytes from the given 'T.Context'. Returns 'Nothing'
-- on EOF.
--
-- Up to @16384@ decrypted bytes will be received at once.
recv :: MonadIO m => T.Context -> m (Maybe B.ByteString)
recv ctx = liftIO $ do
    E.handle (\T.Error_EOF -> return Nothing)
             (do bs <- T.recvData ctx
                 if B.null bs
                    then return Nothing -- I think this never happens
                    else return (Just bs))
{-# INLINABLE recv #-}

-- | Encrypts the given strict 'B.ByteString' and sends it through the
-- 'T.Context'.
send :: MonadIO m => T.Context -> B.ByteString -> m ()
send ctx = \bs -> T.sendData ctx (BL.fromStrict bs)
{-# INLINABLE send #-}

-- | Encrypts the given lazy 'BL.ByteString' and sends it through the
-- 'T.Context'.
sendLazy :: MonadIO m => T.Context -> BL.ByteString -> m ()
sendLazy = T.sendData
{-# INLINE sendLazy #-}

--------------------------------------------------------------------------------

-- | Try to create a new credential object from a public certificate and the
-- associated private key that are stored on the filesystem in PEM format.
credentialLoadX509
  :: MonadIO m
  => FilePath -- ^ Public certificate (X.509 format).
  -> FilePath -- ^ Private key associated with the certificate.
  -> m (Either String T.Credential)
credentialLoadX509 cert key = liftIO $ T.credentialLoadX509 cert key

--------------------------------------------------------------------------------
-- Internal utils

-- | Like 'T.bye' from the "Network.TLS" module, except it ignores 'ePIPE'
-- errors which might happen if the remote peer closes the connection first.
silentBye :: T.Context -> IO ()
silentBye ctx = do
    E.catch (T.bye ctx) $ \e -> case e of
        Eg.IOError{ Eg.ioe_type  = Eg.ResourceVanished
                  , Eg.ioe_errno = Just ioe
                  } | Errno ioe == ePIPE
          -> return ()
        _ -> E.throwIO e

