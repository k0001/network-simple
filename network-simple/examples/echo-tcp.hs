{-# LANGUAGE OverloadedStrings #-}

-- This is an example TCP server that listens on port 9000 and echoes
-- back to clients whatever they send. Incoming connections and handled
-- concurrently.

module Main (main) where

import           Control.Concurrent (forkIO)
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Network.Simple.TCP as S


main :: IO ()
main = S.withSocketsDo $ do
    S.listen "*" "9000" $ \(lsock, laddr) -> do
      putStrLn $ "Listening for TCP connections at " ++ show laddr
      forever . S.acceptFork lsock $ \(csock, caddr) -> do
        putStrLn $ "Accepted incoming connection from " ++ show caddr
        echoloop csock

  where
    echoloop sock = do
      mbs <- S.recv sock 4096
      case mbs of
        Just bs -> S.send sock bs >> echoloop sock
        Nothing -> return ()
