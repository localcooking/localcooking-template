{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , OverloadedLists
  , RecordWildCards
  #-}

module Google.ReCaptcha where

import Data.Text (Text)
import Data.Aeson (FromJSON (..), ToJSON (..), object, (.=), Value (Object), (.:))
import Data.Aeson.Types (typeMismatch)
import Data.URI (URI (..))
import Data.URI.Auth (URIAuth (..))
import Data.URI.Auth.Host (URIAuthHost (Host))
import qualified Data.Strict.Maybe as Strict


newtype ReCaptchaSiteKey = ReCaptchaSiteKey
  { getReCaptchaSiteKey :: Text
  } deriving (Eq, Show, FromJSON, ToJSON)

newtype ReCaptchaSecret = ReCaptchaSecret
  { getReCaptchaSecret :: Text
  } deriving (Eq, Show, FromJSON, ToJSON)




googleReCaptchaAssetURI :: URI
googleReCaptchaAssetURI =
  URI
    (Strict.Just "https")
    True
    (URIAuth Strict.Nothing (Host ["www","google"] "com") Strict.Nothing)
    ["recaptcha", "api.js"]
    []
    Strict.Nothing



newtype ReCaptchaResponse = ReCaptchaResponse
  { getReCaptchaResponse :: Text
  } deriving (Eq, Show, FromJSON, ToJSON)




googleReCaptchaVerifyURI :: URI
googleReCaptchaVerifyURI =
  URI
    (Strict.Just "https")
    True
    (URIAuth Strict.Nothing (Host ["www","google"] "com") Strict.Nothing)
    ["recaptcha", "api", "siteverify"]
    []
    Strict.Nothing


data ReCaptchaVerifyResponse = ReCaptchaVerifyResponse Bool

instance FromJSON ReCaptchaVerifyResponse where
  parseJSON (Object o) = ReCaptchaVerifyResponse <$> o .: "success"
  parseJSON json = typeMismatch "ReCaptchaVerifyResponse" json
