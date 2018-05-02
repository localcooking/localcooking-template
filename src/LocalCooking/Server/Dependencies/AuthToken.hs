{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , OverloadedLists
  , MultiParamTypeClasses
  #-}

module LocalCooking.Server.Dependencies.AuthToken where

import LocalCooking.Types (AppM)
import LocalCooking.Types.Keys (Keys (..))
import LocalCooking.Types.Env (Env (..), Managers (..), TokenContexts (..))
import LocalCooking.Auth (loginAuth)
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Server.Dependencies.AccessToken.Generic (AccessTokenInitIn (..), AccessTokenInitOut (..), AccessTokenDeltaOut (..), accessTokenServer)
import LocalCooking.Database.Query.User (loginWithFB, login, AuthTokenFailure)
import Text.EmailAddress (EmailAddress)
import Facebook.Types (FacebookLoginCode)
import Facebook.Return (handleFacebookLoginReturn)

import Web.Dependencies.Sparrow (Server)
import Data.Aeson (FromJSON (..), ToJSON (..), object, (.=), (.:), Value (..))
import Data.Aeson.Types (typeMismatch)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Newtype (Newtype (unpack, pack))



-- TODO Google ReCaptcha
data AuthTokenInitIn
  = AuthTokenInitInLogin
    { authTokenInitInLoginEmail :: EmailAddress
    , authTokenInitInLoginPassword :: HashedPassword
    }
  | AuthTokenInitInFacebookCode
    { authTokenInitInFacebookCode :: FacebookLoginCode
    }
  | AuthTokenInitInExists
    { authTokenInitInExists :: AuthToken
    }

instance FromJSON AuthTokenInitIn where
  parseJSON json = case json of
    Object o -> do
      let exists = AuthTokenInitInExists <$> o .: "exists"
          code = AuthTokenInitInFacebookCode <$> o .: "fbCode"
          login = AuthTokenInitInLogin <$> o .: "email" <*> o .: "password"
      exists <|> code <|> login
    _ -> fail
    where
      fail = typeMismatch "AuthTokenInitIn" json

instance AccessTokenInitIn AuthTokenInitIn where
  getExists initIn = case initIn of
    AuthTokenInitInExists x -> Just (unpack x)
    _ -> Nothing


data AuthTokenInitOut
  = AuthTokenInitOutSuccess AuthToken
  | AuthTokenInitOutFailure AuthTokenFailure

instance ToJSON AuthTokenInitOut where
  toJSON x = case x of
    AuthTokenInitOutFailure e -> object ["failure" .= e]
    AuthTokenInitOutSuccess y -> object ["success" .= y]

instance AccessTokenInitOut AuthTokenInitOut AuthTokenFailure where
  makeSuccess = AuthTokenInitOutSuccess . pack
  makeFailure = AuthTokenInitOutFailure



data AuthTokenDeltaIn
  = AuthTokenDeltaInLogout -- TODO plus AuthToken...? Tokens are --more-- mutually unique than SIDs?
    -- a session can die, but store the AuthToken in local storage and attempt to use later -
    -- login's discontinuity and session's discontinuity mutually overlay.

instance FromJSON AuthTokenDeltaIn where
  parseJSON json = case json of
    String x | x == "logout" -> pure AuthTokenDeltaInLogout
             | otherwise -> fail
    _ -> fail
    where
      fail = typeMismatch "AuthTokenDeltaIn" json


data AuthTokenDeltaOut
  = AuthTokenDeltaOutRevoked -- remotely logged out

instance ToJSON AuthTokenDeltaOut where
  toJSON x = case x of
    AuthTokenDeltaOutRevoked -> String "revoked"

instance AccessTokenDeltaOut AuthTokenDeltaOut where
  makeRevoke = AuthTokenDeltaOutRevoked



authTokenServer :: Server AppM AuthTokenInitIn
                               AuthTokenInitOut
                               AuthTokenDeltaIn
                               AuthTokenDeltaOut
authTokenServer initIn = do
  Env{envTokenContexts = TokenContexts{tokenContextAuth}} <- ask

  let getAuthToken :: AuthTokenInitIn -> AppM (Either (Maybe AuthTokenFailure) AuthToken)
      getAuthToken initIn' = case initIn' of
        -- invoked remotely from a client whenever casually attempting a normal login
        AuthTokenInitInLogin email password -> do
          Env{envDatabase} <- ask
          eUserId <- liftIO (login envDatabase email password)

          case eUserId of
            Left e -> pure $ Left $ Just e
            Right userId -> do
              authToken <- loginAuth userId
              pure $ Right authToken

        -- invoked remotely from client when started with an authToken in frontendEnv, or in localStorage
        AuthTokenInitInExists _ -> pure (Left Nothing)

        -- invoked on facebookLoginReturn, only when the user exists
        AuthTokenInitInFacebookCode code -> do
          Env
            { envManagers = Managers{managersFacebook}
            , envKeys = Keys{keysFacebook}
            , envHostname
            , envTls
            } <- ask

          eX <- liftIO $ handleFacebookLoginReturn
                  managersFacebook keysFacebook envTls envHostname code
          case eX of
            Left e -> liftIO $ do
              putStr "Facebook error:"
              print e
              pure (Left Nothing)
            Right (fbToken,fbUserId) -> do
              Env{envDatabase} <- ask
              mUserId <- liftIO (loginWithFB envDatabase fbToken fbUserId)
              case mUserId of
                Nothing -> pure (Left Nothing) -- FIXME redirect to registration page with fbUserId field filled
                Just userId -> do
                  authToken <- loginAuth userId
                  pure (Right authToken)

  accessTokenServer tokenContextAuth getAuthToken (\revoke AuthTokenDeltaInLogout -> revoke) (\_ -> pure ()) initIn
