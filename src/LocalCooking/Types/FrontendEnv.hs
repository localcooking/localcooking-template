{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}

module LocalCooking.Types.FrontendEnv where

import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Server.Dependencies.AuthToken (PreliminaryAuthToken)
import Facebook.App (FacebookClientId)
import Facebook.State (FacebookLoginUnsavedFormData)
import Google.ReCaptcha (ReCaptchaSiteKey)

import qualified Data.Text as T
import Data.Aeson (ToJSON (..), (.=), object)



data FrontendEnv = FrontendEnv
  { frontendEnvDevelopment :: Bool
  , frontendEnvFacebookClientID :: FacebookClientId
  , frontendEnvGoogleReCaptchaSiteKey :: ReCaptchaSiteKey
  , frontendEnvAuthToken :: PreliminaryAuthToken
  , frontendEnvFormData :: Maybe FacebookLoginUnsavedFormData
  , frontendEnvSalt :: HashedPassword
  }

instance ToJSON FrontendEnv where
  toJSON
    FrontendEnv
      { frontendEnvDevelopment
      , frontendEnvFacebookClientID
      , frontendEnvGoogleReCaptchaSiteKey
      , frontendEnvAuthToken
      , frontendEnvFormData
      , frontendEnvSalt
      } = object
    [ "development" .= frontendEnvDevelopment
    , "facebookClientID" .= frontendEnvFacebookClientID
    , "googleReCaptchaSiteKey" .= frontendEnvGoogleReCaptchaSiteKey
    , "authToken" .= frontendEnvAuthToken
    , "formData" .= frontendEnvFormData
    , "salt" .= frontendEnvSalt
    ]
