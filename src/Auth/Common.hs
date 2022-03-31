{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Auth.Common where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except
import Crypto.Random.Types
import Crypto.Store.X509
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.PEM
import qualified Data.Text as T
import Data.X509
import qualified Jose.Jwa as Jose
import qualified Jose.Jwk as Jose
import qualified Jose.Jwt as Jose
import qualified Data.Map as M
import Data.Word8
import Safe
import Data.IORef
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Control.Concurrent.Timer
import Control.Concurrent.Suspend

type Certificates = [(T.Text, SignedExact Certificate)]

-- | Validate a token given a list of accepted certificates with public RSA keys.
validateToken :: [(T.Text, SignedExact Certificate)] -> BS.ByteString -> IO (Either String Jose.Jws)
validateToken certs token =
  runExceptT $ do
    let jwks = do
          (keyId, cert) <- certs
          PubKeyRSA pubKey <- [certPubKey $ getCertificate cert]
          pure $
            Jose.RsaPublicJwk
              pubKey
              (Just (Jose.KeyId keyId))
              (Just Jose.Sig)
              (Just $ Jose.Signed Jose.RS256)
    jwtContent <-
      withExceptT show $
      ExceptT $ liftIO $ Jose.decode jwks (Just $ Jose.JwsEncoding Jose.RS256) token
    case jwtContent of
      Jose.Jws (jwsHeader, jwsContent) -> return (jwsHeader, jwsContent)
      _otherwise -> fail "Token does not contain a JWS."

startCertificateUpdateThread :: IO (IORef Certificates)
startCertificateUpdateThread = do
  ref <- newIORef []
  let fetch = fetchCertificates ref
  fetch
  t <- newTimer
  _t <- repeatedStart t fetch (mDelay 55)
  pure ref

fetchCertificates :: IORef Certificates -> IO ()
fetchCertificates ref = do
  print "fetching certs"
  certs <-
    httpJSON
      "https://www.googleapis.com/robot/v1/metadata/x509/securetoken@system.gserviceaccount.com"
  let status = getResponseStatus certs
  unless (status == status200) $
    fail $ "Failed to update certificates" <> show status
  atomicWriteIORef ref $
    [ (k, cert)
    | (k, v) <- M.toList $ getResponseBody certs
    -- TODO (drsk) only allow one cert?
    , cert <- readSignedObjectFromMemory $ BS.fromString v
    ]
