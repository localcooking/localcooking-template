{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}

module LocalCooking.Server.Dependencies.PasswordVerify where

import LocalCooking.Types (AppM)
import LocalCooking.Types.Env (Env (..))
import LocalCooking.Auth (usersAuthToken)
import LocalCooking.Common.AuthToken (AuthToken)
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Database.Query.User (checkPassword,userIdByEmail)

import Web.Dependencies.Sparrow.Types (Server, JSONVoid, staticServer)

import Text.EmailAddress (EmailAddress)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), object, (.=), Value (Object, String))
import Data.Aeson.Types (typeMismatch)
import Control.Applicative ((<|>))
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)


data PasswordVerifyInitIn
  = PasswordVerifyInitInAuth
    { passwordVerifyInitInAuthAuthToken :: AuthToken
    , passwordVerifyInitInAuthPassword  :: HashedPassword
    }
  | PasswordVerifyInitInUnauth
    { passwordVerifyInitInUnauthEmail    :: EmailAddress
    , passwordVerifyInitInUnauthPassword :: HashedPassword
    }

instance FromJSON PasswordVerifyInitIn where
  parseJSON json = case json of
    Object o -> do
      let auth = do
            authToken <- o .: "authToken"
            password <- o .: "password"
            pure PasswordVerifyInitInAuth
              { passwordVerifyInitInAuthAuthToken = authToken
              , passwordVerifyInitInAuthPassword = password
              }
          unauth = do
            email <- o .: "email"
            password <- o .: "password"
            pure PasswordVerifyInitInUnauth
              { passwordVerifyInitInUnauthEmail = email
              , passwordVerifyInitInUnauthPassword = password
              }
      auth <|> unauth
    _ -> fail
    where
      fail = typeMismatch "PasswordVerifyInitIn" json


data PasswordVerifyInitOut
  = PasswordVerifyInitOutNoAuth
  | PasswordVerifyInitOutSuccess
  | PasswordVerifyInitOutFailure

instance ToJSON PasswordVerifyInitOut where
  toJSON x = case x of
    PasswordVerifyInitOutNoAuth -> String "no-auth"
    PasswordVerifyInitOutSuccess -> String "success"
    PasswordVerifyInitOutFailure -> String "failure"


passwordVerifyServer :: Server AppM PasswordVerifyInitIn
                               PasswordVerifyInitOut
                               JSONVoid
                               JSONVoid
passwordVerifyServer = staticServer $ \initIn -> do
  Env{envDatabase} <- ask

  (mUserId,password) <- case initIn of
    PasswordVerifyInitInAuth authToken password' -> do
      x <- usersAuthToken authToken
      pure (x,password')
    PasswordVerifyInitInUnauth email password' -> do
      x <- liftIO (userIdByEmail envDatabase email)
      pure (x,password')

  case mUserId of
    Nothing -> pure $ Just PasswordVerifyInitOutNoAuth
    Just userId -> do
      b <- liftIO $ checkPassword envDatabase userId password
      if b
        then pure $ Just PasswordVerifyInitOutSuccess
        else pure $ Just PasswordVerifyInitOutFailure
