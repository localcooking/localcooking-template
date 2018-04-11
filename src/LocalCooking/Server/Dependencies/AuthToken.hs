{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , OverloadedLists
  #-}

module LocalCooking.Server.Dependencies.AuthToken where

import LocalCooking.Types (AppM)
import LocalCooking.Types.Keys (Keys (..))
import LocalCooking.Types.Env (Env (..), Managers (..), isDevelopment)
import LocalCooking.Auth (loginAuth, logoutAuth, usersAuthToken)
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Common.AuthToken (AuthToken)
import LocalCooking.Database.Query.User (loginWithFB, login, AuthTokenFailure)
import Text.EmailAddress (EmailAddress)
import Facebook.Types (FacebookLoginCode)
import Facebook.Return (handleFacebookLoginReturn)

import Web.Dependencies.Sparrow (Server, ServerContinue (..), ServerReturn (..), ServerArgs (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (FromJSON (..), ToJSON (..), object, (.=), (.:), Value (..))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Strict.Maybe as Strict
import Data.Strict.Tuple (Pair (..))
import Data.Monoid ((<>))
import Data.URI (URI (..), printURI)
import Data.Singleton.Class (Extractable (runSingleton))
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import qualified Control.Monad.Trans.Control.Aligned as Aligned
import Control.Exception.Safe (throwM)
import Control.Logging (log', warn')
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMapMVar.Hash as TMapMVar
import Network.HTTP.Types.URI (Query)
import Network.HTTP.Client (httpLbs, responseBody, parseRequest)



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

data AuthTokenInitOut
  = AuthTokenInitOutSuccess AuthToken
  | AuthTokenInitOutFailure AuthTokenFailure

instance ToJSON AuthTokenInitOut where
  toJSON x = case x of
    AuthTokenInitOutFailure e -> object ["failure" .= e]
    AuthTokenInitOutSuccess y -> object ["success" .= y]


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
  = AuthTokenDeltaOutNew AuthToken
  | AuthTokenDeltaOutRevoked -- remotely logged out

instance ToJSON AuthTokenDeltaOut where
  toJSON x = case x of
    AuthTokenDeltaOutRevoked -> String "revoked"
    AuthTokenDeltaOutNew token -> object ["new" .= token]



authTokenServer :: Server AppM AuthTokenInitIn
                               AuthTokenInitOut
                               AuthTokenDeltaIn
                               AuthTokenDeltaOut
authTokenServer initIn = do
  Env{envAuthTokenExpire,envDatabase} <- ask
  let serverReturnSuccess authToken = ServerContinue
        { serverOnUnsubscribe = pure ()
        , serverContinue = \_ -> pure ServerReturn
          { serverInitOut = AuthTokenInitOutSuccess authToken
          , serverOnOpen = \ServerArgs{serverSendCurrent} -> do
              thread <- Aligned.liftBaseWith $ \runInBase -> async $ do
                () <- atomically $ TMapMVar.lookup envAuthTokenExpire authToken
                fmap runSingleton $ runInBase $ serverSendCurrent AuthTokenDeltaOutRevoked
              pure (Just thread)
          , serverOnReceive = \ServerArgs{serverDeltaReject} r -> case r of
              AuthTokenDeltaInLogout -> do
                serverDeltaReject
                logoutAuth authToken
          }
        }

  case initIn of
    -- invoked remotely from a client whenever casually attempting a normal login
    AuthTokenInitInLogin email password -> do
      eUserId <- liftIO $ login envDatabase email password

      case eUserId of
        Left e -> pure $ Just ServerContinue
          { serverOnUnsubscribe = pure ()
          , serverContinue = \_ -> pure ServerReturn
            { serverInitOut = AuthTokenInitOutFailure e
            , serverOnOpen = \ServerArgs{serverDeltaReject} -> do
                serverDeltaReject
                pure Nothing
            , serverOnReceive = \_ _ -> pure ()
            }
          }
        Right userId -> do
          authToken <- loginAuth userId
          pure $ Just $ serverReturnSuccess authToken

    -- invoked remotely from client when started with an authToken in frontendEnv, or in localStorage
    AuthTokenInitInExists authToken -> do
      mUser <- usersAuthToken authToken
      case mUser of
        Nothing -> pure Nothing
        Just _ -> pure $ Just $ serverReturnSuccess authToken

    -- invoked on facebookLoginReturn, only when the user exists
    AuthTokenInitInFacebookCode code -> do
      env@Env
        { envManagers = Managers{managersFacebook}
        , envKeys = Keys{keysFacebook}
        , envHostname
        , envTls
        } <- ask

      eX <- liftIO $ handleFacebookLoginReturn managersFacebook keysFacebook envTls envHostname code
      case eX of
        Left e -> liftIO $ do
          putStr "Facebook error:"
          print e
          pure Nothing
        Right (fbToken,fbUserId) -> do
          mUserId <- liftIO $ loginWithFB envDatabase fbToken fbUserId
          case mUserId of
            Nothing -> pure Nothing -- FIXME redirect to registration page with fbUserId field filled
            Just userId -> do
              authToken <- loginAuth userId
              pure $ Just $ serverReturnSuccess authToken
