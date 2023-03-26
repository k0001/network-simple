{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as B
import           Data.Char (toUpper)
import           Data.Monoid ((<>))
import           Data.X509 (SignedCertificate)
import           Data.X509.CertificateStore (makeCertificateStore, CertificateStore)
import           Data.X509.File (readSignedObject)
import qualified Network.Simple.TCP.TLS as Z
import qualified Network.Socket as NS
import qualified Network.TLS as T
import           System.Console.GetOpt
import           System.Environment (getProgName, getArgs)

server :: T.Credential -> Z.HostPreference -> NS.ServiceName
       -> Maybe CertificateStore -> IO ()
server cred hp port ycs = do
    let ss = Z.makeServerSettings cred ycs
    Z.serve ss hp port $ \(ctx,caddr) -> do
       putStrLn $ show caddr <> " joined."
       consume ctx $ Z.send ctx . B.map toUpper
       putStrLn $ show caddr <> " quit."
  where

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
        server (optServerCredentials opts) (Z.Host hostname) port
               (makeCertificateStore <$> optCACert opts)
      (_,_,msgs) -> do
        pn <- getProgName
        let header = "Usage: " <> pn <> " [OPTIONS] HOSTNAME PORT"
        error $ concat msgs ++ usageInfo header options

--------------------------------------------------------------------------------
-- The boring stuff below is related to command line parsing

data Options = Options
  { optServerCertFile     :: FilePath
  , optServerKeyFile      :: FilePath
  , optServerCredentials  :: T.Credential
  , optCACert             :: Maybe [SignedCertificate]
  }

defaultOptions :: Options
defaultOptions = Options
  { optServerCertFile = error "Missing optServerCertFile"
  , optServerKeyFile = error "Missing optServerKeyFile"
  , optServerCredentials = undefined
  , optCACert = Nothing
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option [] ["cert"]   (ReqArg readServerCert "FILE") "Server certificate"
  , Option [] ["key"]    (ReqArg readServerCredentials "FILE") "Server private key"
  , Option [] ["cacert"] (OptArg readCACert     "FILE")
    "CA certificate to verify a client certificate, if given"
  ]

readServerCert :: FilePath -> Options -> IO Options
readServerCert fp opt =
  return opt {optServerCertFile = fp}

readServerCredentials :: FilePath -> Options -> IO Options
readServerCredentials arg opt = do
  ec <- T.credentialLoadX509 (optServerCertFile opt) arg
  case ec of
    Left err -> error err
    Right c ->
      return $ opt { optServerCredentials = c
                   , optServerKeyFile = arg }

readCACert :: Maybe FilePath -> Options -> IO Options
readCACert Nothing    opt = return opt
readCACert (Just arg) opt = do
    certs <- readSignedObject arg
    return $ opt { optCACert = Just certs }

