{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}

module LocalCooking.Types.Keys where

import Data.Aeson (FromJSON (..), Value (Object), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T
import Facebook.App (Credentials)
import Google.Keys (GoogleCredentials)



data Keys = Keys
  { keysFacebook :: Credentials
  , keysGoogle :: GoogleCredentials
  }



instance FromJSON Keys where
  parseJSON (Object o) = do
    keysFacebook <- o .: "facebook"
    keysGoogle <- o .: "google"
    pure Keys{keysFacebook,keysGoogle}
  parseJSON x = typeMismatch "Keys" x
