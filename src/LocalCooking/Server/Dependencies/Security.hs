{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}

module LocalCooking.Server.Dependencies.Security where

import LocalCooking.Types (AppM)
import LocalCooking.Types.Env (Env (..))
import LocalCooking.Auth (usersAuthToken)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Database.Query.User (changeSecurityDetails)

import Web.Dependencies.Sparrow.Types (Server, JSONVoid, staticServer)

import Text.EmailAddress (EmailAddress)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), object, (.=), Value (Object, String))
import Data.Aeson.Types (typeMismatch)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)


data SecurityInitIn = SecurityInitIn
  { securityInitInAuthToken    :: AuthToken
  , securityInitInEmailAddress :: EmailAddress
  , securityInitInNewPassword  :: HashedPassword
  , securityInitInOldPassword  :: HashedPassword
  }

instance FromJSON SecurityInitIn where
  parseJSON json = case json of
    Object o -> do
      authToken <- o .: "authToken"
      email <- o .: "email"
      newPassword <- o .: "newPassword"
      oldPassword <- o .: "oldPassword"
      pure SecurityInitIn
        { securityInitInAuthToken = authToken
        , securityInitInEmailAddress = email
        , securityInitInNewPassword = newPassword
        , securityInitInOldPassword = oldPassword
        }
    _ -> fail
    where
      fail = typeMismatch "SecurityInitIn" json


data SecurityInitOut
  = SecurityInitOutNoAuth
  | SecurityInitOutSuccess
  | SecurityInitOutFailure

instance ToJSON SecurityInitOut where
  toJSON x = case x of
    SecurityInitOutNoAuth -> String "no-auth"
    SecurityInitOutSuccess -> String "success"
    SecurityInitOutFailure -> String "failure"


securityServer :: Server AppM SecurityInitIn
                               SecurityInitOut
                               JSONVoid
                               JSONVoid
securityServer = staticServer $ \(SecurityInitIn authToken email newPassword oldPassword) -> do
  Env{envDatabase} <- ask

  mUserId <- usersAuthToken authToken
  case mUserId of
    Nothing -> pure $ Just SecurityInitOutNoAuth
    Just userId -> do
      b <- liftIO $ changeSecurityDetails envDatabase userId (email,newPassword) oldPassword
      if b
        then pure $ Just SecurityInitOutSuccess
        else pure $ Just SecurityInitOutFailure
