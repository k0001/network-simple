{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- | Simple tools for establishing and using Secure WebSockets connections on
-- top of TLS (i.e, @wss:\/\/@).
--
-- See the
-- [network-simple-ws](https://hackage.haskell.org/package/network-simple-ws)
-- package for insecure WebSockets (i.e, @ws:\/\/@) support.
--
-- Notice that, currently, this is package offers tools that are mostly
-- intreresting from a client's point of view. Server side support will come
-- later.
module Network.Simple.WSS
 ( -- * Sending and receiving
   WS.Connection
 , WS.recv
 , WS.send
 , WS.close
   -- * Client side
 , connect
 , connectOverSOCKS5
   -- * Low level
 , WS.clientConnectionFromStream
 , streamFromContext
 ) where


import qualified Control.Concurrent.Async as Async
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Exception.Safe as Ex
import qualified Data.ByteString as B
import Data.Foldable (traverse_)

import qualified Network.Simple.TCP.TLS as T
import qualified Network.Simple.WS as WS
import qualified Network.WebSockets as W
import qualified Network.WebSockets.Connection as W (pingThread)
import qualified Network.WebSockets.Stream as W (Stream, makeStream, close)

--------------------------------------------------------------------------------

-- | Connect to the specified Secure WebSockets server.
connect
  :: (MonadIO m, Ex.MonadMask m)
  => T.ClientParams  -- ^ TLS settings.
  -> T.HostName
  -- ^ Secure WebSockets server host name (e.g., @\"www.example.com\"@ or IP
  -- address).
  -> T.ServiceName
  -- ^ Secure WebSockets server port (e.g., @\"443\"@ or @\"www\"@).
  -> B.ByteString
  -- ^ Secure WebSockets resource (e.g., @\"/foo\/qux?bar=wat&baz\"@).
  --
  -- Leading @\'\/\'@ is optional.
  -> [(B.ByteString, B.ByteString)]
  -- ^ Extra HTTP Headers
  -- (e.g., @[(\"Authorization\", \"Basic dXNlcjpwYXNzd29yZA==\")]@).
  -> ((W.Connection, T.SockAddr) -> m r)
  -- ^ Computation to run after establishing a Secure WebSockets to the remote
  -- server. Takes the WebSockets connection and remote end address.
  -> m r
connect cs hn sn res hds act = do
  T.connect cs hn sn $ \(ctx, saddr) -> do
     Ex.bracket (streamFromContext ctx) (liftIO . W.close) $ \stream -> do
        conn <- WS.clientConnectionFromStream stream hn sn res hds
        withAsync (W.pingThread conn 30 (pure ())) $ \_ -> 
          act (conn, saddr)

-- | Like 'connect', but connects to the destination server through a SOCKS5
-- proxy.
connectOverSOCKS5
  :: (MonadIO m, Ex.MonadMask m)
  => T.HostName -- ^ SOCKS5 proxy server hostname or IP address.
  -> T.ServiceName -- ^ SOCKS5 proxy server service port name or number.
  -> T.ClientParams -- ^ TLS settings.
  -> T.HostName
  -- ^ Destination Secure WebSockets server hostname or IP address. We connect
  -- to this host /through/ the SOCKS5 proxy specified in the previous
  -- arguments.
  --
  -- Note that if hostname resolution on this 'T.HostName' is necessary, it
  -- will happen on the proxy side for security reasons, not locally.
  -> T.ServiceName
  -- ^ Destination Secure WebSockets server port (e.g., @\"443\"@ or @\"www\"@).
  -> B.ByteString
  -- ^ WebSockets resource (e.g., @\"/foo\/qux?bar=wat&baz\"@).
  --
  -- Leading @\'\/\'@ is optional.
  -> [(B.ByteString, B.ByteString)]
  -- ^ Extra HTTP Headers
  -- (e.g., @[(\"Authorization\", \"Basic dXNlcjpwYXNzd29yZA==\")]@).
  -> ((W.Connection, T.SockAddr, T.SockAddr) -> m r)
  -- ^ Computation taking a 'W.Connection' for communicating with the
  -- destination Secure WebSockets server through the SOCKS5 server, the address
  -- of that SOCKS5 server, and the address of the destination WebSockets
  -- server, in that order.
 -> m r
connectOverSOCKS5 phn psn tcs dhn dsn res hds act = do
  T.connectOverSOCKS5 phn psn tcs dhn dsn $ \(ctx, pa, da) -> do
    Ex.bracket (streamFromContext ctx) (liftIO . W.close) $ \stream -> do
      conn <- WS.clientConnectionFromStream stream dhn dsn res hds
      withAsync (W.pingThread conn 30 (pure ())) $ \_ -> 
        act (conn, pa, da)

-- | Obtain a 'W.Stream' implemented using the given TLS 'T.Context'. You can
-- use the
-- [network-simple-tls](https://hackage.haskell.org/package/network-simple-tls)
-- library to get one of those.
streamFromContext :: MonadIO m => T.Context -> m W.Stream
streamFromContext ctx = liftIO $ do
  W.makeStream (T.recv ctx) (traverse_ (T.sendLazy ctx))

-- | Like 'Async.async', but generalized to 'Ex.MonadMask' and 'MonadIO'.
withAsync
  :: (Ex.MonadMask m, MonadIO m) 
  => IO a 
  -> (Async.Async a -> m b) 
  -> m b
withAsync io = Ex.bracket
  (liftIO $ Async.asyncWithUnmask (\u -> u io))
  (liftIO . Async.uninterruptibleCancel)

