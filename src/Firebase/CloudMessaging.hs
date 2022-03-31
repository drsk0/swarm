{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Firebase.CloudMessaging where

import Control.Concurrent.Suspend
import Control.Concurrent.Timer
import Crypto.PubKey.RSA.Types
import qualified Data.Aeson as Aeson
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.HashMap.Strict as HMS
import Data.Either
import Data.IORef
import qualified Data.Text as T
import Data.X509
import Data.X509.Memory (readKeyFileFromMemory)
import GHC.Generics
import Jose.Jwa as Jose
import Jose.Jws as Jose
import Jose.Jwt as Jose
import Network.HTTP.Simple
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Data.Time.Clock.System

data ServiceAccount = ServiceAccount {
  project_id :: T.Text
  , private_key_id :: T.Text
  , private_key :: T.Text
  , client_email :: T.Text
  , client_id :: T.Text
  , auth_uri :: T.Text
  , token_uri :: T.Text
  , auth_provider_x509_cert_url :: T.Text
  , client_x509_cert_url :: T.Text
} deriving (Generic, Aeson.FromJSON)

authRequestToken :: FilePath -> IO Jose.Jwt
authRequestToken fp = do
  bs <- BSL.readFile fp
  case Aeson.eitherDecode bs of
    Left err ->
      fail $ "Failed to parse Google service account .json file: " <> show err
    Right ServiceAccount {..} -> do
      case readKeyFileFromMemory $ T.encodeUtf8 private_key of
        [PrivKeyRSA privateKey] -> do
          now <- getCurrentTime
          let now' = fromIntegral $ systemSeconds $ utcToSystemTime now
          jwtOrErr <-
            Jose.rsaEncode RS256 privateKey $
            BSL.toStrict $
            Aeson.encode $
            GoogleAuthReq
              { iss = client_email
              , scope = "https://www.googleapis.com/auth/firebase.messaging"
              , aud = token_uri
              , exp = now' + 60 * 60
              , iat = now'
              }
          case jwtOrErr of
            Left err -> fail $ show err
            Right jwt -> pure jwt
        _other ->
          fail
            "Failed to parse private key from Google service account .json file."

type Token = T.Text

startServiceAuthUpdateThread :: FilePath -> IO (IORef Token)
startServiceAuthUpdateThread serviceAccountJsonFp = do
  ref <- newIORef T.empty
  let requestTok = requestToken serviceAccountJsonFp ref
  requestTok
  t <- newTimer
  _t <- repeatedStart t requestTok (mDelay 55)
  pure ref

requestToken :: FilePath -> IORef Token -> IO ()
requestToken serviceAccountJsonFp ref = do
  print "requesting token"
  tokenJwt <- authRequestToken serviceAccountJsonFp
  token <- authRequestToken serviceAccountJsonFp
  req <- parseRequest "POST https://oauth2.googleapis.com/token"
  let req1 =
        setRequestBodyJSON
          GoogleTokenReq
            { grant_type = "urn:ietf:params:oauth:grant-type:jwt-bearer"
            , assertion = token
            }
          req
  res <- httpJSON req1
  case getResponseBody res of
    Aeson.Object m ->
      case HMS.lookup "access_token" m of
        Nothing -> responseFail res
        Just (Aeson.String newToken) -> do
          print newToken
          writeIORef ref newToken
        Just _somethingElse -> responseFail res
    _otherwise -> responseFail res
  where
    responseFail res =
      fail $ "Access token request failed. Received " <> show res

data GoogleAuthReq =
  GoogleAuthReq
    { iss :: T.Text
    , scope :: T.Text
    , aud :: T.Text
    , exp :: Integer
    , iat :: Integer
    }
  deriving (Generic, Aeson.ToJSON)

data GoogleTokenReq =
  GoogleTokenReq
    { grant_type :: T.Text
    , assertion :: Jose.Jwt
    }
  deriving (Generic, Aeson.ToJSON)

data FcmMessage =
  FcmMessage
    { validate_only :: Bool
    , message :: Message
    }
  deriving (Generic, Aeson.ToJSON)

data Message =
  Message
    { name :: T.Text
    , notification :: Notification
    , token :: T.Text
    }
  deriving (Generic, Aeson.ToJSON)

data Notification =
  Notification
    { title :: T.Text
    , body :: T.Text
    }
  deriving (Generic, Aeson.ToJSON)
