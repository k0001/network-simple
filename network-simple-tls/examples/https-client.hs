{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL ()
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.X509 (SignedCertificate)
import           Data.X509.CertificateStore (makeCertificateStore, CertificateStore)
import           Data.X509.Validation (FailedReason(..))
import           Data.X509.File (readSignedObject)
import qualified Network.Simple.TCP.TLS as Z
import qualified Network.Socket as NS
import qualified Network.TLS as T
import           System.X509 (getSystemCertificateStore)
import           System.Console.GetOpt
import           System.Environment (getProgName, getArgs)

client :: CertificateStore -> Z.Credentials -> NS.HostName
       -> NS.ServiceName -> IO ()
client cStore creds host port = do
  let csettings = Z.makeClientSettings (host, B.pack (':' : port)) creds cStore
  Z.connect csettings host port $ \(ctx,_) -> do
     Z.send ctx "GET / HTTP/1.0\r\n\r\n"
     consume ctx B.putStr >> putStrLn ""

-- | Repeatedly receive data from the given 'T.Context' until exhausted,
-- performing the given action on each received chunk.
consume :: T.Context -> (B.ByteString -> IO ()) -> IO ()
consume ctx k = do
    mbs <- Z.recv ctx
    case mbs of
      Nothing -> return ()
      Just bs -> k bs >> consume ctx k


main :: IO ()
main = Z.withSocketsDo $ do
    args <- getArgs
    case getOpt RequireOrder options args of
      (actions, [hostname,port], _) -> do
        opts <- foldl (>>=) (return defaultOptions) actions
        cStore <- case optCACert opts of
          Nothing -> getSystemCertificateStore
          Just ca -> return $ makeCertificateStore ca
        client cStore (fromMaybe (T.Credentials []) $ optClientCredentials opts) hostname port
      (_,_,msgs) -> do
        pn <- getProgName
        let header = "Usage: " <> pn <> " [OPTIONS] HOSTNAME PORT"
        error $ concat msgs ++ usageInfo header options

--------------------------------------------------------------------------------
-- The boring stuff below is related to command line parsing

data Options = Options
  { optClientCertFile     :: Maybe FilePath
  , optClientKeyFile      :: Maybe FilePath
  , optClientCredentials  :: Maybe T.Credentials
  , optCACert             :: Maybe [SignedCertificate]
  }

defaultOptions :: Options
defaultOptions = Options
  { optClientCertFile    = Nothing
  , optClientKeyFile     = Nothing
  , optClientCredentials = Nothing
  , optCACert            = Nothing
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option [] ["cert"]   (OptArg readClientCert "FILE") "Client certificate"
  , Option [] ["key"]    (OptArg readClientCredentials "FILE") "Client private key"
  , Option [] ["cacert"] (OptArg readCACert     "FILE") "CA certificate"
  ]

readClientCert :: Maybe FilePath -> Options -> IO Options
readClientCert Nothing    opt = return opt
readClientCert fp opt =
  return opt {optClientCertFile = fp}

readClientCredentials :: Maybe FilePath -> Options -> IO Options
readClientCredentials Nothing opt = return opt
readClientCredentials arg@(Just fp) opt = do
  let certFile = fromMaybe (error "Client certificate missing") $
                 optClientCertFile opt
  ec <- T.credentialLoadX509 certFile fp
  case ec of
    Left err ->
      error err
    Right c ->
      return $ opt { optClientCredentials = Just $ T.Credentials [c]
                   , optClientKeyFile = arg
                   }

readCACert :: Maybe FilePath -> Options -> IO Options
readCACert Nothing    opt = return opt
readCACert (Just arg) opt = do
    certs <- readSignedObject arg
    return $ opt { optCACert = Just certs }
