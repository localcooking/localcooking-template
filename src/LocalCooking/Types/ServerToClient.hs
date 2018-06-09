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
import LocalCooking.Dependencies.AuthToken (PreliminaryAuthToken)
import LocalCooking.Semantics.Common (ConfirmEmailError)
import Facebook.Types (FacebookClientId)
import Facebook.State (FacebookLoginUnsavedFormData)
import Google.Keys (ReCaptchaSiteKey)

import Data.Aeson (ToJSON (..), (.=), object)


-- | Data that gets passed to every client, as its environment
data ServerToClient = ServerToClient
  { serverToClientDevelopment            :: Bool
  , serverToClientFacebookClientId       :: FacebookClientId -- ^ For Facebook login
  , serverToClientGoogleReCaptchaSiteKey :: ReCaptchaSiteKey -- ^ Public ReCaptcha API key
  , serverToClientConfirmEmail           :: Maybe ConfirmEmailError
  , serverToClientAuthToken              :: Maybe PreliminaryAuthToken -- ^ Parsed from @?authToken=...@ query string
  , serverToClientFormData               :: Maybe FacebookLoginUnsavedFormData -- ^ Parsed from @?formData=...@ query string
  , serverToClientSalt                   :: HashedPassword -- ^ Public, static, 32-bit password salt
  }

instance ToJSON ServerToClient where
  toJSON
    ServerToClient
      { serverToClientDevelopment
      , serverToClientFacebookClientId
      , serverToClientGoogleReCaptchaSiteKey
      , serverToClientConfirmEmail
      , serverToClientAuthToken
      , serverToClientFormData
      , serverToClientSalt
      } = object
    [ "development" .= serverToClientDevelopment
    , "facebookClientId" .= serverToClientFacebookClientId
    , "googleReCaptchaSiteKey" .= serverToClientGoogleReCaptchaSiteKey
    , "confirmEmail" .= serverToClientConfirmEmail
    , "authToken" .= serverToClientAuthToken
    , "formData" .= serverToClientFormData
    , "salt" .= serverToClientSalt
    ]
