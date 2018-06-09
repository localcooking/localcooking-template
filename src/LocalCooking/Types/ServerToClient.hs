{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}

{-|

Module: LocalCooking.Types.ServerToClient
Copyright: (c) 2018 Local Cooking Inc.
License: Proprietary
Maintainer: athan.clark@localcooking.com
Portability: GHC

-}

module LocalCooking.Types.ServerToClient where

import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Common.AccessToken.Email (EmailToken)
import LocalCooking.Dependencies.AuthToken (PreliminaryAuthToken)
import Facebook.Types (FacebookClientId)
import Facebook.State (FacebookLoginUnsavedFormData)
import Google.Keys (ReCaptchaSiteKey)

import Data.Aeson (ToJSON (..), (.=), object)


-- | Data that gets passed to every client, as its environment
data ServerToClient = ServerToClient
  { serverToClientDevelopment            :: Bool
  , serverToClientFacebookClientID       :: FacebookClientId -- ^ For Facebook login
  , serverToClientGoogleReCaptchaSiteKey :: ReCaptchaSiteKey -- ^ Public ReCaptcha API key
  , serverToClientEmailToken             :: Maybe EmailToken
  , serverToClientAuthToken              :: Maybe PreliminaryAuthToken -- ^ Parsed from @?authToken=...@ query string
  , serverToClientFormData               :: Maybe FacebookLoginUnsavedFormData -- ^ Parsed from @?formData=...@ query string
  , serverToClientSalt                   :: HashedPassword -- ^ Public, static, 32-bit password salt
  }

instance ToJSON ServerToClient where
  toJSON
    ServerToClient
      { serverToClientDevelopment
      , serverToClientFacebookClientID
      , serverToClientGoogleReCaptchaSiteKey
      , serverToClientEmailToken
      , serverToClientAuthToken
      , serverToClientFormData
      , serverToClientSalt
      } = object
    [ "development" .= serverToClientDevelopment
    , "facebookClientID" .= serverToClientFacebookClientID
    , "googleReCaptchaSiteKey" .= serverToClientGoogleReCaptchaSiteKey
    , "emailToken" .= serverToClientEmailToken
    , "authToken" .= serverToClientAuthToken
    , "formData" .= serverToClientFormData
    , "salt" .= serverToClientSalt
    ]
