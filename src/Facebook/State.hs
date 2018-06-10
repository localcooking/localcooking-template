{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , FlexibleContexts
  , UndecidableInstances
  #-}

{-|

Module: Facebook.State
Copyright: (c) 2018 Local Cooking Inc.
License: Proprietary
Maintainer: athan.clark@localcooking.com
Portability: GHC

-}

module Facebook.State where

import LocalCooking.Semantics.Common (SocialLoginForm)

import Data.Aeson (ToJSON (..), FromJSON (..), object, (.=), (.:), Value (Object, String))
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.Attoparsec (attoAeson)
import Data.Text (Text)
import Control.Applicative ((<|>))
import Path.Extended (Location, locationParser, printLocation)



-- TODO other social login user id's as unsaved data
-- | Unsaved form data intended to remain intact during a login redirection cycle.
data FacebookLoginUnsavedFormData
  = -- | Register page
    FacebookLoginUnsavedFormDataRegister
    { facebookLoginUnsavedFormDataRegisterEmail        :: Text
    , facebookLoginUnsavedFormDataRegisterEmailConfirm :: Text
    , facebookLoginUnsavedFormDataRegisterSocialLogin  :: SocialLoginForm
    }
  | -- | Security page
    FacebookLoginUnsavedFormDataSecurity
    { facebookLoginUnsavedFormDataSecurityEmail        :: Text
    , facebookLoginUnsavedFormDataSecurityEmailConfirm :: Text
    , facebookLoginUnsavedFormDataSecuritySocialLogin  :: SocialLoginForm
    }

instance ToJSON FacebookLoginUnsavedFormData where
  toJSON x = case x of
    FacebookLoginUnsavedFormDataRegister email emailConfirm socialLogin -> object
      ["register" .= object ["email" .= email, "emailConfirm" .= emailConfirm, "socialLogin" .= socialLogin]]
    FacebookLoginUnsavedFormDataSecurity email emailConfirm socialLogin -> object
      ["security" .= object ["email" .= email, "emailConfirm" .= emailConfirm, "socialLogin" .= socialLogin]]

instance FromJSON FacebookLoginUnsavedFormData where
  parseJSON json = case json of
    Object o -> do
      let register = do
            o' <- o .: "register"
            FacebookLoginUnsavedFormDataRegister <$> o' .: "email" <*> o' .: "emailConfirm" <*> o' .: "socialLogin"
          security = do
            o' <- o .: "security"
            FacebookLoginUnsavedFormDataSecurity <$> o' .: "email" <*> o' .: "emailConfirm" <*> o' .: "socialLogin"
      register <|> security
    _ -> fail
    where
      fail = typeMismatch "FacebookLoginUnsavedFormData" json



-- | Complete @{state: ...}@ parameters passed to-and-from a facebook login redirection cycle
data FacebookLoginState = FacebookLoginState
  { facebookLoginStateOrigin   :: Location -- ^ Origin
  , facebookLoginStateFormData :: Maybe FacebookLoginUnsavedFormData -- ^ Potentially unsaved form data
  }

instance ToJSON FacebookLoginState where
  toJSON FacebookLoginState{..} = object
    [ "origin" .= (String $ printLocation facebookLoginStateOrigin)
    , "formData" .= facebookLoginStateFormData
    ]

instance FromJSON FacebookLoginState where
  parseJSON json = case json of
    Object o -> do
      json' <- o .: "origin"
      loc <- attoAeson locationParser json'
      FacebookLoginState loc <$> o .: "formData"
    _ -> fail
    where
      fail = typeMismatch "FacebookLoginState" json
