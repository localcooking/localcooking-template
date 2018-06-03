{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}

{-|

Module: LocalCooking.Types.FrontendEnv
Copyright: (c) 2018 Local Cooking Inc.
License: Proprietary
Maintainer: athan.clark@localcooking.com
Portability: GHC

-}

module LocalCooking.Types.FrontendEnv where

import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Common.AccessToken.Email (EmailToken)
import LocalCooking.Dependencies.AuthToken (PreliminaryAuthToken)
import Facebook.Types (FacebookClientId)
import Facebook.State (FacebookLoginUnsavedFormData)
import Google.Keys (ReCaptchaSiteKey)

import Data.Aeson (ToJSON (..), (.=), object)


-- | Data that gets passed to every client, as its environment
data FrontendEnv = FrontendEnv
  { frontendEnvDevelopment            :: Bool
  , frontendEnvFacebookClientID       :: FacebookClientId -- ^ For Facebook login
  , frontendEnvGoogleReCaptchaSiteKey :: ReCaptchaSiteKey -- ^ Public ReCaptcha API key
  , frontendEnvEmailToken             :: Maybe EmailToken
  , frontendEnvAuthToken              :: PreliminaryAuthToken -- ^ Parsed from @?authToken=...@ query string
  , frontendEnvFormData               :: Maybe FacebookLoginUnsavedFormData -- ^ Parsed from @?authToken=...@ query string
  , frontendEnvSalt                   :: HashedPassword -- ^ Public, static, 32-bit password salt
  }

instance ToJSON FrontendEnv where
  toJSON
    FrontendEnv
      { frontendEnvDevelopment
      , frontendEnvFacebookClientID
      , frontendEnvGoogleReCaptchaSiteKey
      , frontendEnvEmailToken
      , frontendEnvAuthToken
      , frontendEnvFormData
      , frontendEnvSalt
      } = object
    [ "development" .= frontendEnvDevelopment
    , "facebookClientID" .= frontendEnvFacebookClientID
    , "googleReCaptchaSiteKey" .= frontendEnvGoogleReCaptchaSiteKey
    , "emailToken" .= frontendEnvEmailToken
    , "authToken" .= frontendEnvAuthToken
    , "formData" .= frontendEnvFormData
    , "salt" .= frontendEnvSalt
    ]
