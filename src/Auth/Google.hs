{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Auth.Google where

import Data.Aeson
import Data.Either
import qualified Data.Text as T
import Data.Time.Clock
import GHC.Generics
import Prelude hiding (exp)
import Data.Time.Clock.System

data GoogleJws = GoogleJws
  { name :: Maybe T.Text
  , picture :: Maybe T.Text
  , iss :: T.Text
  , aud :: T.Text
  , auth_time :: Integer
  , user_id :: T.Text
  , sub :: T.Text
  , iat :: Integer
  , exp :: Integer
  , email :: Maybe T.Text
  , email_verified :: Maybe Bool
  , firebase :: FirebaseData
  } deriving Generic

instance FromJSON GoogleJws

data FirebaseData = FirebaseData
  { identities :: FirebaseIdentities
  , sign_in_provider :: T.Text
  } deriving Generic

instance FromJSON FirebaseData

data FirebaseIdentities = FirebaseIdentities
  { googleCom :: Maybe [String]
  , firebase_email :: Maybe [T.Text]
  }
instance FromJSON FirebaseIdentities where
  parseJSON = withObject "FirebaseIdentities" $ \o ->
    FirebaseIdentities
    <$> o .:? "google.com"
    <*> o .:? "email"

validateJws :: GoogleJws -> UTCTime -> Maybe [String]
validateJws GoogleJws {aud, iss, sub, exp, iat, auth_time} now
  | null failedChecks = Nothing
  | otherwise = Just failedChecks
  where
    (failedChecks, _) = partitionEithers checks
    check :: String -> Bool -> Either String ()
    check msg p
      | p = Right ()
      | not p = Left msg
    checks =
      [ check "wrong aud field" $ aud == "swarm-90059"
      , check "wrong iss field" $
        iss == "https://securetoken.google.com/swarm-90059"
      , check "sub field is empty" $ not $ T.null sub
      , check "exp field in the past" $ timeDiff exp now > 0
      , check "iat field in the future" $ timeDiff iat now <= 0
      , check "auth_time in the future" $ timeDiff auth_time now <= 0
      ]
    timeDiff t1 t2 =
      diffUTCTime
        (systemToUTCTime $
         MkSystemTime {systemSeconds = fromInteger t1, systemNanoseconds = 0})
        t2
