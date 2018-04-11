{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}

module LocalCooking.Types.FrontendEnv where

import LocalCooking.Common.AuthToken (AuthToken)
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Auth.Error (AuthError, PreliminaryAuthToken)
import Facebook.App (FacebookClientId)
import Google.ReCaptcha (ReCaptchaSiteKey)

import qualified Data.Text as T
import Data.Aeson (ToJSON (..), (.=), object)



data FrontendEnv = FrontendEnv
  { frontendEnvDevelopment :: Bool
  , frontendEnvFacebookClientID :: FacebookClientId
  , frontendEnvGoogleReCaptchaSiteKey :: ReCaptchaSiteKey
  , frontendEnvAuthToken :: PreliminaryAuthToken
  , frontendEnvSalt :: HashedPassword
  }

instance ToJSON FrontendEnv where
  toJSON
    FrontendEnv
      { frontendEnvDevelopment
      , frontendEnvFacebookClientID
      , frontendEnvGoogleReCaptchaSiteKey
      , frontendEnvAuthToken
      , frontendEnvSalt
      } = object
    [ "development" .= frontendEnvDevelopment
    , "facebookClientID" .= frontendEnvFacebookClientID
    , "googleReCaptchaSiteKey" .= frontendEnvGoogleReCaptchaSiteKey
    , "authToken" .= frontendEnvAuthToken
    , "salt" .= frontendEnvSalt
    ]
