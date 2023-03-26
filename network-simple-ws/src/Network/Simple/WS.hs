{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Simple tools for establishing and using insecure WebSockets connections on top of
-- TCP (i.e, @ws:\/\/@).
--
-- See the
-- [network-simple-wss](https://hackage.haskell.org/package/network-simple-wss)
-- package for Secure WebSockets (i.e, @wss:\/\/@) support.
--
-- Notice that, currently, this is package offers tools that are mostly
-- intreresting from a client's point of view. Server side support will come
-- later.
module Network.Simple.WS
 ( -- * Sending and receiving
   W.Connection
 , recv
 , send
 , close
   -- * Client side
 , connect
 , connectSOCKS5
   -- * Low level
 , clientConnectionFromStream
 , streamFromSocket
 ) where


import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Exception.Safe as Ex
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.Foldable (traverse_)
import Data.Word
import GHC.IO.Exception as IO

import qualified Network.Simple.TCP as T
import qualified Network.WebSockets as W
import qualified Network.WebSockets.Connection as W (pingThread)
import qualified Network.WebSockets.Stream as W (Stream, makeStream, close)

--------------------------------------------------------------------------------

-- | Connect to the specified WebSockets server.
connect
  :: (MonadIO m, Ex.MonadMask m)
  => T.HostName
  -- ^ WebSockets server host name (e.g., @\"www.example.com\"@ or IP
  -- address).
  -> T.ServiceName
  -- ^ WebSockets server port (e.g., @\"443\"@ or @\"www\"@).
  -> B.ByteString
  -- ^ WebSockets resource (e.g., @\"/foo\/qux?bar=wat&baz\"@).
  --
  -- Leading @\'\/\'@ is optional.
  -> [(B.ByteString, B.ByteString)]
  -- ^ Extra HTTP Headers
  -- (e.g., @[(\"Authorization\", \"Basic dXNlcjpwYXNzd29yZA==\")]@).
  -> ((W.Connection, T.SockAddr) -> m r)
  -- ^ Computation to run after establishing a WebSockets to the remote
  -- server. Takes the WebSockets 'W.Connection' and remote end address.
  -> m r
connect hn sn res hds act = do
  T.connect hn sn $ \(sock, saddr) -> do
    Ex.bracket (streamFromSocket sock) (liftIO . W.close) $ \stream -> do
      conn <- clientConnectionFromStream stream hn sn res hds
      withAsync (W.pingThread conn 30 (pure ())) $ \_ -> 
        act (conn, saddr)

-- | Like 'connect', but connects to the destination server through a SOCKS5
-- proxy.
connectSOCKS5
  :: (MonadIO m, Ex.MonadMask m)
  => T.HostName 
  -- ^ SOCKS5 proxy server hostname or IP address.
  -> T.ServiceName 
  -- ^ SOCKS5 proxy server service port name or number.
  -> T.HostName
  -- ^ Destination WebSockets server hostname or IP address. We connect to this
  -- host /through/ the SOCKS5 proxy specified in the previous arguments.
  --
  -- Note that if hostname resolution on this 'T.HostName' is necessary, it
  -- will happen on the proxy side for security reasons, not locally.
  -> T.ServiceName
  -- ^ Destination WebSockets server port (e.g., @\"443\"@ or @\"www\"@).
  -> B.ByteString
  -- ^ WebSockets resource (e.g., @\"/foo\/qux?bar=wat&baz\"@).
  --
  -- Leading @\'\/\'@ is optional.
  -> [(B.ByteString, B.ByteString)]
  -- ^ Extra HTTP Headers
  -- (e.g., @[(\"Authorization\", \"Basic dXNlcjpwYXNzd29yZA==\")]@).
  -> ((W.Connection, T.SockAddr, T.SockAddr) -> m r)
  -- ^ Computation taking a 'W.Connection' for communicating with the
  -- destination WebSockets server through the SOCKS5 server, the address
  -- of that SOCKS5 server, and the address of the destination WebSockets
  -- server, in that order.
 -> m r
connectSOCKS5 phn psn dhn dsn res hds act = do
  T.connectSOCKS5 phn psn dhn dsn $ \(sock, pa, da) -> do
    Ex.bracket (streamFromSocket sock) (liftIO . W.close) $ \stream -> do
      conn <- clientConnectionFromStream stream dhn dsn res hds
      withAsync (W.pingThread conn 30 (pure ())) $ \_ -> 
        act (conn, pa, da)

-- | Obtain a 'W.Connection' to the specified 'Uri' over the given 'W.Stream',
-- connected to either a WebSockets server, or a Secure WebSockets server.
clientConnectionFromStream
  :: MonadIO m
  => W.Stream 
  -- ^ Stream on which to establish the WebSockets connection.
  -> T.HostName
  -- ^ WebSockets server host name (e.g., @\"www.example.com\"@ or IP address).
  -> T.ServiceName 
  -- ^ WebSockets server port (e.g., @\"443\"@ or @\"www\"@).
  -> B.ByteString
  -- ^ WebSockets resource (e.g., @\"/foo\/qux?bar=wat&baz\"@).
  --
  -- Leading @\'\/\'@ is optional.
  -> [(B.ByteString, B.ByteString)]
  -- ^ Extra HTTP Headers
  -- (e.g., @[(\"Authorization\", \"Basic dXNlcjpwYXNzd29yZA==\")]@).
  -> m W.Connection 
  -- ^ Established WebSockets connection
clientConnectionFromStream stream hn sn res hds = liftIO $ do
  let res' :: String = '/' : dropWhile (=='/') (B8.unpack res)
      hds' :: W.Headers = map (first CI.mk) hds
      hnsn :: String = hn ++ ":" ++ sn
      wopts :: W.ConnectionOptions = W.defaultConnectionOptions
        { W.connectionStrictUnicode =
            False -- Slows stuff down. And see 'recv'.
        , W.connectionCompressionOptions =
            W.PermessageDeflateCompression
              W.defaultPermessageDeflate }
  W.newClientConnection stream hnsn res' wopts hds'

-- | Obtain a 'W.Stream' implemented using the network 'T.Socket'. You can
-- use the
-- [network-simple](https://hackage.haskell.org/package/network-simple)
-- library to get one of those.
streamFromSocket :: MonadIO m => T.Socket -> m W.Stream
streamFromSocket sock = liftIO $ do
  W.makeStream (T.recv sock 4096) (traverse_ (T.sendLazy sock))

-- | Receive a single full WebSockets message from the remote end as a lazy
-- 'BL.ByteString' (potentially 'BL.empty').
--
-- Throws 'IO.IOException' if there is an unexpected 'W.Connection' error.
--
-- If the remote end requested the 'W.Connection' to be closed, then 'Left'
-- will be returned instead, with a close code and reason description.
--
-- * See https://datatracker.ietf.org/doc/html/rfc6455#section-7.4 for details
-- about the close codes.
--
-- * Do not use 'recv' after receiving a close request.
--
-- * If you receive a close request after after having sent a close request 
-- yourself (see 'close'), then the WebSocket 'W.Connection' is 
-- considered closed and you can proceed to close the underlying transport. 
--
-- * If you didn't send a close request before, then you may continue to use 
-- 'send', but you are expected to perform 'close' as soon as possible in order 
-- to indicate a graceful closing of the connection. 

-- Note: The WebSockets protocol supports the silly idea of sending text
-- rather than bytes. We don't support that. If necessary, you can find support 
-- for this in the `websockets` library.
recv :: MonadIO m 
     => W.Connection 
     -> m (Either (Word16, BL.ByteString) BL.ByteString)
recv conn = liftIO $ Ex.try (W.receiveDataMessage conn) >>= \case
  Right (W.Binary !bl) -> pure $ Right bl
  Right (W.Text !bl _) -> pure $ Right bl
  Left (W.CloseRequest !w !bl) -> pure $ Left (w, bl)
  Left W.ConnectionClosed -> 
    Ex.throw $ ioe "recv" IO.ResourceVanished "Connection closed"
  Left (W.ParseException s) -> 
    Ex.throw $ ioe "recv" IO.ProtocolError ("WebSocket parsing error: " <> s)
  Left (W.UnicodeException s) -> 
    Ex.throw $ ioe "recv" IO.ProtocolError ("WebSocket UTF-8 error: " <> s)

-- | Send a lazy 'BL.ByteString' (potentially 'BL.empty') to the remote end as 
-- a single WebSockets message, in potentially multiple frames.
--
-- If there is an issue with the 'W.Connection', an exception originating from
-- the underlying 'W.Stream' will be thrown.

-- Note: The WebSockets protocol supports the silly idea of sending text rather
-- than bytes. We don't support that. If necessary, users can
-- find support for this in the `websockets` library.
send :: MonadIO m => W.Connection -> BL.ByteString -> m ()
send conn = liftIO . W.sendDataMessage conn . W.Binary

-- | Send a close request to the remote end.
--
-- After sending this request you should not use 'send' anymore, but you
-- should still continue to call 'recv' to process any pending incomming 
-- messages. As soon as 'recv' returns 'Left', you can consider the WebSocket 
-- 'W.Connection' closed and can proceed to close the underlying transport.
-- 
-- If there is an issue with the 'W.Connection', an exception originating from
-- the underlying 'W.Stream' will be thrown.
close :: MonadIO m 
      => W.Connection 
      -> Word16        -- ^ Close code.
      -> BL.ByteString -- ^ Reason for closing.
      -> m ()
close conn w bl = liftIO $ W.sendCloseCode conn w bl

-- | Like 'Async.async', but generalized to 'Ex.MonadMask' and 'MonadIO'.
withAsync
  :: (Ex.MonadMask m, MonadIO m) 
  => IO a 
  -> (Async a -> m b) 
  -> m b
withAsync io = Ex.bracket
  (liftIO $ Async.asyncWithUnmask (\u -> u io))
  (liftIO . Async.uninterruptibleCancel)

-- | Construct a 'IO.IOError' relevant to this module.
ioe :: String  -- ^ Location
    -> IO.IOErrorType 
    -> String  -- ^ Description
    -> IO.IOError
ioe l t s = IO.IOError
  { IO.ioe_type = t
  , IO.ioe_location = "Network.Simple.WS." <> l 
  , IO.ioe_description = s
  , IO.ioe_errno = Nothing
  , IO.ioe_handle = Nothing
  , IO.ioe_filename = Nothing
  }
