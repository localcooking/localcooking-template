{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , FlexibleContexts
  , UndecidableInstances
  #-}

module Facebook.State where


import Data.Aeson (ToJSON (..), FromJSON (..), object, (.=), (.:), Value (Object, String))
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.Attoparsec (attoAeson)
import Data.Text (Text)
import Control.Applicative ((<|>))
import Path.Extended (ToLocation (toLocation), FromLocation (parseLocation), locationParser, printLocation)



-- TODO other social login user id's as unsaved data
data FacebookLoginUnsavedFormData
  = FacebookLoginUnsavedFormDataRegister
    { facebookLoginUnsavedFormDataRegisterEmail        :: Text
    , facebookLoginUnsavedFormDataRegisterEmailConfirm :: Text
    }
  | FacebookLoginUnsavedFormDataSecurity
    { facebookLoginUnsavedFormDataSecurityEmail        :: Text
    , facebookLoginUnsavedFormDataSecurityEmailConfirm :: Text
    }

instance ToJSON FacebookLoginUnsavedFormData where
  toJSON x = case x of
    FacebookLoginUnsavedFormDataRegister email emailConfirm -> object
      ["register" .= object ["email" .= email, "emailConfirm" .= emailConfirm]]
    FacebookLoginUnsavedFormDataSecurity email emailConfirm -> object
      ["security" .= object ["email" .= email, "emailConfirm" .= emailConfirm]]

instance FromJSON FacebookLoginUnsavedFormData where
  parseJSON json = case json of
    Object o -> do
      let register = do
            o' <- o .: "register"
            FacebookLoginUnsavedFormDataRegister <$> o' .: "email" <*> o' .: "emailConfirm"
          security = do
            o' <- o .: "security"
            FacebookLoginUnsavedFormDataSecurity <$> o' .: "email" <*> o' .: "emailConfirm"
      register <|> security
    _ -> fail
    where
      fail = typeMismatch "FacebookLoginUnsavedFormData" json



data FacebookLoginState siteLinks = FacebookLoginState
  { facebookLoginStateOrigin   :: siteLinks
  , facebookLoginStateFormData :: Maybe FacebookLoginUnsavedFormData
  }

instance ToLocation siteLinks => ToJSON (FacebookLoginState siteLinks) where
  toJSON FacebookLoginState{..} = object
    [ "origin" .= (String $ printLocation $ toLocation facebookLoginStateOrigin)
    , "formData" .= facebookLoginStateFormData
    ]

instance FromLocation siteLinks => FromJSON (FacebookLoginState siteLinks) where
  parseJSON json = case json of
    Object o -> do
      json' <- o .: "origin"
      loc <- attoAeson locationParser json'
      case parseLocation loc of
        Left _ -> fail
        Right x -> FacebookLoginState x <$> o .: "formData"
    _ -> fail
    where
      fail = typeMismatch "FacebookLoginState" json
