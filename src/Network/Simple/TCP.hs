{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module exports functions that abstract simple TCP 'NS.Socket'
-- usage patterns.
--
-- This module uses 'MonadIO' and 'Ex.MonadMask' extensively so that you can
-- reuse these functions in monads other than 'IO'. However, if you don't care
-- about any of that, just pretend you are using the 'IO' monad all the time
-- and everything will work as expected.

-- Some code in this file was adapted from the @pipes-network@ library by
-- Renzo Carbonara. Copyright (c) 2012-2013. See its licensing terms (BSD3) at:
--   https://github.com/k0001/pipes-network/blob/master/LICENSE
--
-- Some code in this file was adapted from the @network-conduit@ library by
-- Michael Snoyman. Copyright (c) 2011. See its licensing terms (BSD3) at:
--   https://github.com/snoyberg/conduit/blob/master/network-conduit/LICENSE

module Network.Simple.TCP (
  -- * Introduction to TCP networking
  -- $tcp-101

  -- * Client side
  -- $client-side
    connect

  -- * Server side
  -- $server-side
  , serve
  -- ** Listening
  , listen
  -- ** Accepting
  , accept
  , acceptFork

  -- * Utils
  , recv
  , send
  , sendLazy
  , sendMany

  -- * Low level support
  , bindSock
  , listenSock
  , connectSock
  , closeSock

  -- * Note to Windows users
  , NS.withSocketsDo

  -- * Types
  , HostPreference(..)
  -- ** Re-exported from @Network.Socket@
  , NS.HostName
  , NS.ServiceName
  , NS.Socket
  , NS.SockAddr
  ) where

import           Control.Concurrent             (ThreadId, forkIO)
import qualified Control.Exception              as Ex (getMaskingState)
import qualified Control.Exception.Safe         as Ex
import           Control.Monad
import           Control.Monad.IO.Class         (MonadIO(liftIO))
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BSL
import           Data.List                      (transpose, partition)
import qualified Network.Socket                 as NS
import qualified Network.Socket.ByteString      as NSB
import qualified Network.Socket.ByteString.Lazy as NSBL
import qualified System.IO                      as IO
import           System.Timeout                 (timeout)

import Network.Simple.Internal
  (HostPreference(..), hpHostName, isIPv4addr, isIPv6addr, ipv4mapped_to_ipv4,
   prioritize, happyEyeballSort)

--------------------------------------------------------------------------------
-- $tcp-101
--
-- This introduction aims to give you a overly simplified overview of some
-- concepts you need to know about TCP sockets in order to make effective use of
-- this module.
--
-- There are two ends in a single TCP connection: one is the TCP «server» and
-- the other is the TCP «client». Each end is uniquely identified by an IP
-- address and a TCP port pair, and each end knows the IP address and TCP port
-- of the other end. Each end can send and receive data to and from the other
-- end.
--
-- A TCP server, once «bound» to a well-known IP address and TCP port, starts
-- «listening» for incoming connections from TCP clients to such bound IP
-- address and TCP port. When a TCP client attempts to connect to the TCP
-- server, the TCP server must «accept» the incoming connection in order to
-- start exchanging data with the remote end. A single TCP server can
-- sequentially accept many incoming connections, possibly handling each one
-- concurrently.
--
-- A TCP client can «connect» to a well-known IP address and TCP port previously
-- bound by a listening TCP server willing to accept new incoming connections.
-- Once the connection is established, the TCP client can immediately start
-- exchanging data with the TCP server. The TCP client is randomly assigned a
-- TCP port when connecting, and its IP address is selected by the operating
-- system so that it is reachable from the remote end.
--
-- The TCP client a and the TCP server can be running in the same host or in
-- different hosts.

--------------------------------------------------------------------------------

-- $client-side
--
-- Here's how you could run a TCP client:
--
-- @
-- 'connect' \"www.example.org\" \"80\" $ \\(connectionSocket, remoteAddr) -> do
--   putStrLn $ \"Connection established to \" ++ show remoteAddr
--   -- Now you may use connectionSocket as you please within this scope,
--   -- possibly using 'recv' and 'send' to interact with the remote end.
-- @

-- | Connect to a TCP server and use the connection.
--
-- The connection socket is shut down and closed when done or in case of
-- exceptions.
--
-- If you prefer to acquire and close the socket yourself, then use
-- 'connectSock' and 'closeSock'.
connect
  :: (MonadIO m, Ex.MonadMask m)
  => NS.HostName -- ^ Server hostname or IP address.
  -> NS.ServiceName -- ^ Server service port name or number.
  -> ((NS.Socket, NS.SockAddr) -> m r)
  -- ^ Computation taking the communication socket and the server address.
  -> m r
connect host port = Ex.bracket (connectSock host port) (closeSock . fst)

--------------------------------------------------------------------------------

-- $server-side
--
-- Here's how you can run a TCP server that handles in different threads each
-- incoming connection to port @8000@ at IPv4 address @127.0.0.1@:
--
-- @
-- 'serve' ('Host' \"127.0.0.1\") \"8000\" $ \\(connectionSocket, remoteAddr) -> do
--   putStrLn $ \"TCP connection established from \" ++ show remoteAddr
--   -- Now you may use connectionSocket as you please within this scope,
--   -- possibly using 'recv' and 'send' to interact with the remote end.
-- @
--
-- If you need more control on the way your server runs, then you can use more
-- advanced functions such as 'listen', 'accept' and 'acceptFork'.

--------------------------------------------------------------------------------

-- | Start a TCP server that accepts incoming connections and handles them
-- concurrently in different threads.
--
-- Any acquired sockets are properly shut down and closed when done or in case
-- of exceptions. Exceptions from the threads handling the individual
-- connections won't cause 'serve' to die.
--
-- Note: This function performs 'listen', 'acceptFork', so don't perform
-- those manually.
serve
  :: MonadIO m
  => HostPreference -- ^ Host to bind.
  -> NS.ServiceName -- ^ Server service port name or number to bind.
  -> ((NS.Socket, NS.SockAddr) -> IO ())
  -- ^ Computation to run in a different thread once an incoming connection is
  -- accepted. Takes the connection socket and remote end address.
  -> m ()
serve hp port k = liftIO $ do
    listen hp port $ \(lsock, _) -> do
       forever $ Ex.catch
          (void (acceptFork lsock k))
          (\se -> IO.hPutStrLn IO.stderr (x ++ show (se :: Ex.SomeException)))
  where
    x :: String
    x = "Network.Simple.TCP.serve: Synchronous exception accepting connection: "

--------------------------------------------------------------------------------

-- | Bind a TCP listening socket and use it.
--
-- The listening socket is closed when done or in case of exceptions.
--
-- If you prefer to acquire and close the socket yourself, then use 'bindSock'
-- and 'closeSock', as well as 'listenSock' function.
--
-- Note: The 'NS.NoDelay' and 'NS.ReuseAddr' options are set on the socket. The
-- maximum number of incoming queued connections is 2048.
listen
  :: (MonadIO m, Ex.MonadMask m)
  => HostPreference -- ^ Host to bind.
  -> NS.ServiceName -- ^ Server service port name or number to bind.
  -> ((NS.Socket, NS.SockAddr) -> m r)
  -- ^ Computation taking the listening socket and the address it's bound to.
  -> m r
listen hp port = Ex.bracket
   (do x@(bsock,_) <- bindSock hp port
       listenSock bsock (max 2048 NS.maxListenQueue)
       pure x)
   (closeSock . fst)

-- | Listen for new connections of the given bound socket.
listenSock
  :: MonadIO m
  => NS.Socket  -- ^ Bound socket.
  -> Int
  -- ^ Maximum number of incoming queued connections (we suggest @2048@ if you
  -- don't have an opinion).
  -> m ()
listenSock bsock qs = liftIO (NS.listen bsock qs)

--------------------------------------------------------------------------------

-- | Accept a single incoming connection and use it.
--
-- The connection socket is shut down and closed when done or in case of
-- exceptions.
accept
  :: (MonadIO m, Ex.MonadMask m)
  => NS.Socket -- ^ Listening and bound socket.
  -> ((NS.Socket, NS.SockAddr) -> m r)
  -- ^ Computation to run once an incoming connection is accepted. Takes the
  -- connection socket and remote end address.
  -> m r
accept lsock k = Ex.mask $ \restore -> do
  (csock, addr) <- restore (liftIO (NS.accept lsock))
  Ex.onException
     (restore (k (csock, ipv4mapped_to_ipv4 addr)))
     (closeSock csock)
{-# INLINABLE accept #-}

-- | Accept a single incoming connection and use it in a different thread.
--
-- The connection socket is shut down and closed when done or in case of
-- exceptions.
acceptFork
  :: MonadIO m
  => NS.Socket -- ^ Listening and bound socket.
  -> ((NS.Socket, NS.SockAddr) -> IO ())
  -- ^ Computation to run in a different thread once an incoming connection is
  -- accepted. Takes the connection socket and remote end address.
  -> m ThreadId
acceptFork lsock k = liftIO $ Ex.mask $ \restore -> do
  (csock, addr) <- restore (NS.accept lsock)
  Ex.onException
     (forkIO (Ex.finally (restore (k (csock, ipv4mapped_to_ipv4 addr)))
                         (closeSock csock)))
     (closeSock csock)
{-# INLINABLE acceptFork #-}

--------------------------------------------------------------------------------

-- | Obtain a 'NS.Socket' connected to the given host and TCP service port.
--
-- The obtained 'NS.Socket' should be closed manually using 'closeSock' when
-- it's not needed anymore, otherwise you risk having the connection and socket
-- open for much longer than needed.
--
-- Prefer to use 'connect' if you will be using the socket within a limited
-- scope and would like it to be closed immediately after its usage or in case
-- of exceptions.
connectSock
  :: MonadIO m
  => NS.HostName -- ^ Server hostname or IP address.
  -> NS.ServiceName -- ^ Server service port name or number.
  -> m (NS.Socket, NS.SockAddr) -- ^ Connected socket and server address.
connectSock host port = liftIO $ do
    addrs <- NS.getAddrInfo (Just hints) (Just host) (Just port)
    tryAddrs (happyEyeballSort addrs)
  where
    hints :: NS.AddrInfo
    hints = NS.defaultHints
      { NS.addrFlags = [NS.AI_ADDRCONFIG]
      , NS.addrSocketType = NS.Stream }
    tryAddrs :: [NS.AddrInfo] -> IO (NS.Socket, NS.SockAddr)
    tryAddrs = \case
      [] -> fail "Network.Simple.TCP.connectSock: No addresses available"
      [x] -> useAddr x
      (x:xs) -> Ex.catch (useAddr x) (\(_ :: IOError) -> tryAddrs xs)
    useAddr :: NS.AddrInfo -> IO (NS.Socket, NS.SockAddr)
    useAddr addr = do
       yx <- timeout 1000000 $ do -- 1 second
          Ex.bracketOnError (newSocket addr) closeSock $ \sock -> do
             let sockAddr = NS.addrAddress addr
             NS.connect sock sockAddr
             pure (sock, sockAddr)
       case yx of
          Nothing -> fail "Network.Simple.TCP.connectSock: Timeout on connect"
          Just x -> pure x

-- | Obtain a 'NS.Socket' bound to the given host name and TCP service port.
--
-- The obtained 'NS.Socket' should be closed manually using 'closeSock' when
-- it's not needed anymore.
--
-- Prefer to use 'listen' if you will be listening on this socket and using it
-- within a limited scope, and would like it to be closed immediately after its
-- usage or in case of exceptions.
--
-- Note: The 'NS.NoDelay' and 'NS.ReuseAddr' options are set on the socket.
bindSock
  :: MonadIO m
  => HostPreference -- ^ Host to bind.
  -> NS.ServiceName -- ^ Server service port name or number to bind.
  -> m (NS.Socket, NS.SockAddr) -- ^ Bound socket and address.
bindSock hp port = liftIO $ do
    addrs <- NS.getAddrInfo (Just hints) (hpHostName hp) (Just port)
    tryAddrs $ case hp of
       HostIPv4 -> prioritize isIPv4addr addrs
       HostIPv6 -> prioritize isIPv6addr addrs
       HostAny  -> prioritize isIPv6addr addrs
       _        -> addrs
  where
    hints :: NS.AddrInfo
    hints = NS.defaultHints
      { NS.addrFlags = [NS.AI_PASSIVE]
      , NS.addrSocketType = NS.Stream }
    tryAddrs :: [NS.AddrInfo] -> IO (NS.Socket, NS.SockAddr)
    tryAddrs = \case
      [] -> fail "bindSock: No addresses available"
      [x] -> useAddr x
      (x:xs) -> Ex.catch (useAddr x) (\(_ :: IOError) -> tryAddrs xs)
    useAddr :: NS.AddrInfo -> IO (NS.Socket, NS.SockAddr)
    useAddr addr = Ex.bracketOnError (newSocket addr) closeSock $ \sock -> do
      let sockAddr = NS.addrAddress addr
      NS.setSocketOption sock NS.NoDelay 1
      NS.setSocketOption sock NS.ReuseAddr 1
      when (isIPv6addr addr) $ do
         NS.setSocketOption sock NS.IPv6Only (if hp == HostIPv6 then 1 else 0)
      NS.bind sock sockAddr
      pure (sock, sockAddr)

-- | Shuts down and closes the 'NS.Socket', silently ignoring any synchronous
-- exception that might happen.
closeSock :: MonadIO m => NS.Socket -> m ()
closeSock s = liftIO $ do
  Ex.catch (Ex.finally (NS.shutdown s NS.ShutdownBoth)
                       (NS.close s))
           (\(_ :: Ex.SomeException) -> pure ())

--------------------------------------------------------------------------------
-- Utils

-- | Read up to a limited number of bytes from a socket.
--
-- Returns `Nothing` if the remote end closed the connection or end-of-input was
-- reached. The number of returned bytes might be less than the specified limit,
-- but it will never 'BS.null'.
recv :: MonadIO m => NS.Socket -> Int -> m (Maybe BS.ByteString)
recv sock nbytes = liftIO $ do
  bs <- liftIO (NSB.recv sock nbytes)
  if BS.null bs
     then pure Nothing
     else pure (Just bs)
{-# INLINABLE recv #-}

-- | Writes a 'BS.ByteString' to the socket.
send :: MonadIO m => NS.Socket -> BS.ByteString -> m ()
send sock = \bs -> liftIO (NSB.sendAll sock bs)
{-# INLINABLE send #-}

-- | Writes a lazy 'BSL.ByteString' to the socket.
sendLazy :: MonadIO m => NS.Socket -> BSL.ByteString -> m ()
{-# INLINABLE sendLazy #-}
#if !MIN_VERSION_network(2,7,0) && defined(mingw32_HOST_OS)
sendLazy sock = \lbs -> sendMany sock (BSL.toChunks lbs) -- see #13.
#else
sendLazy sock = \lbs -> liftIO (NSBL.sendAll sock lbs)
#endif

-- | Writes the given list of 'BS.ByteString's to the socket.
-- This is faster than sending them individually.
sendMany :: MonadIO m => NS.Socket -> [BS.ByteString] -> m ()
sendMany sock = \bs -> liftIO (NSB.sendMany sock bs)
{-# INLINABLE sendMany #-}

--------------------------------------------------------------------------------
-- Misc

newSocket :: NS.AddrInfo -> IO NS.Socket
newSocket addr = NS.socket (NS.addrFamily addr)
                           (NS.addrSocketType addr)
                           (NS.addrProtocol addr)

