{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}

{-|

Module: LocalCooking.Types.Keys
Copyright: (c) 2018 Local Cooking Inc.
License: Proprietary
Maintainer: athan.clark@localcooking.com
Portability: GHC

-}

module LocalCooking.Types.Keys where

import Data.Aeson (FromJSON (..), Value (Object), (.:))
import Data.Aeson.Types (typeMismatch)
import Facebook.App (Credentials)
import Google.Keys (GoogleCredentials)
import SparkPost.Keys (SparkPostCredentials)


-- | Data stored in @~/.localcooking/secret@
data Keys = Keys
  { keysFacebook :: Credentials
  , keysGoogle :: GoogleCredentials
  , keysSparkPost :: SparkPostCredentials
  }


instance FromJSON Keys where
  parseJSON (Object o) = do
    keysFacebook <- o .: "facebook"
    keysGoogle <- o .: "google"
    keysSparkPost <- o .: "sparkPost"
    pure Keys{keysFacebook,keysGoogle,keysSparkPost}
  parseJSON x = typeMismatch "Keys" x
