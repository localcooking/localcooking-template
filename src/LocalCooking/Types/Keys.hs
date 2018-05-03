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


-- | Data stored in @~/.localcooking/secret@
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
