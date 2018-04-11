{-# LANGUAGE
    OverloadedStrings
  #-}

module Google.Keys where

import Google.Analytics (GoogleAnalyticsGTag)
import Google.ReCaptcha (ReCaptchaSiteKey, ReCaptchaSecret)

import Data.Aeson (FromJSON (..), (.:), Value (Object))
import Data.Aeson.Types (typeMismatch)


data GoogleCredentials = GoogleCredentials
  { googleAnalytics :: GoogleAnalyticsGTag
  , googleReCaptcha :: ReCaptchaSiteKey
  , googleReCaptchaSecret :: ReCaptchaSecret
  } deriving (Eq, Show)


instance FromJSON GoogleCredentials where
  parseJSON json = case json of
    Object o -> GoogleCredentials <$> o .: "analytics"
                                  <*> o .: "reCaptcha"
                                  <*> o .: "reCaptchaSecret"
    _ -> fail
    where
      fail = typeMismatch "GoogleCredentials" json
