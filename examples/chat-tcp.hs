{-# LANGUAGE OverloadedStrings #-}

-- This is an example chat TCP server that listens on port 9000 and broadcast
-- incomming messages to every connected client.
-- Messages are treated as UTF-8 encoded text.

module Main (main) where

import           Control.Concurrent.STM       (STM, atomically)
import           Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan
                                              ,readTChan, dupTChan)
import           Control.Concurrent           (forkIO, killThread)
import           Control.Exception            (finally)
import           Control.Monad                (forever, when)
import           Data.Char                    (isSpace)
import           Data.Monoid                  ((<>))
import qualified Data.Text                    as T
import           Data.Text.Encoding           (decodeUtf8, encodeUtf8)
import           Network.Simple.TCP           (listen, acceptFork)
import           Network.Socket               (Socket, SockAddr)
import           Network.Socket.ByteString    (recv, sendAll)


main :: IO ()
main = do
   bchan <- newTChanIO :: IO (TChan T.Text)
            -- ^XXX we should really use 'newBroadcastTCHanIO' from STM-2.4
   listen "*" "9000" $ \(lsock, laddr) -> do
     putStrLn $ "Listening for TCP connections at " ++ show laddr
     forever . acceptFork lsock $ \(csock,caddr) -> do
       putStrLn $ "Accepted incoming connection from " ++ show caddr
       let talk s = writeTChan bchan $ T.pack (show caddr) <> " " <> s <> "\r\n"
           sendText = sendAll csock . encodeUtf8
           recvText = return . decodeUtf8 =<< recv csock 4096
                      -- ^XXX we don't handle messages longer than 4096 bytes!
       atomically $ talk "joined."
       rochan <- atomically $ dupTChan bchan
       finally (handleClient talk rochan sendText recvText)
               (atomically $ talk "gone.")
       putStrLn $ "Closing connection from " ++ show caddr


handleClient :: (T.Text -> STM ()) -- ^Broadcast a message to all chat users.
             -> TChan T.Text       -- ^Incomming chat messages.
             -> (T.Text -> IO ())  -- ^Send text to the client.
             -> IO T.Text          -- ^Receive text from the client.
             -> IO ()
handleClient talk inbox sendText recvText = do
    tid <- forkIO . forever $ atomically (readTChan inbox) >>= sendText
    fromClient
    killThread tid
  where
    fromClient = do
      t <- return . T.strip =<< recvText
      when (not (T.null t)) $ do
        atomically (talk $ "says: " <> t) >> fromClient


