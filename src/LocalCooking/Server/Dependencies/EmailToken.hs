{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , OverloadedLists
  , MultiParamTypeClasses
  , DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Server.Dependencies.EmailToken where

import LocalCooking.Types (AppM)
import LocalCooking.Types.Keys (Keys (..))
import LocalCooking.Types.Env (Env (..), Managers (..), TokenContexts (..))
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Common.AccessToken.Email (EmailToken)
import LocalCooking.Server.Dependencies.AccessToken.Generic (AccessTokenInitIn (..), AccessTokenInitOut (..), AccessTokenDeltaOut (..), accessTokenServer, revokeAccess, lookupAccess)
import LocalCooking.Database.Query.User (loginWithFB, login, LoginFailure, removePendingEmail)
import Text.EmailAddress (EmailAddress)
import Facebook.Types (FacebookLoginCode, FacebookUserId)
import Facebook.Return (FacebookLoginReturnError, handleFacebookLoginReturn)

import Web.Dependencies.Sparrow (Server)
import Web.Dependencies.Sparrow.Types (JSONVoid, staticServer)
import Data.Aeson (FromJSON (..), ToJSON (..), object, (.=), (.:), Value (..))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Newtype (Newtype (unpack, pack))
import Control.Concurrent.STM (atomically)
import Test.QuickCheck (Arbitrary (..), oneof)
import GHC.Generics (Generic)



-- TODO Google ReCaptcha
data EmailTokenInitIn
  = EmailTokenInitInExists
    { emailTokenInitInExists :: EmailToken
    }

instance FromJSON EmailTokenInitIn where
  parseJSON json = case json of
    Object o -> do
      let exists = EmailTokenInitInExists <$> o .: "exists"
      exists
    _ -> fail
    where
      fail = typeMismatch "EmailTokenInitIn" json



data EmailTokenInitOut
  = EmailTokenInitOutSuccess

instance ToJSON EmailTokenInitOut where
  toJSON x = case x of
    EmailTokenInitOutSuccess -> String "success"


emailTokenServer :: Server AppM EmailTokenInitIn
                                EmailTokenInitOut
                                JSONVoid
                                JSONVoid
emailTokenServer = staticServer $ \(EmailTokenInitInExists emailToken) -> do
  Env
    { envTokenContexts = TokenContexts{tokenContextEmail}
    , envDatabase
    } <- ask

  mUserId <- liftIO $ lookupAccess tokenContextEmail emailToken
  case mUserId of
    Nothing -> pure Nothing
    Just userId -> do
      liftIO $ do
        atomically (revokeAccess tokenContextEmail emailToken)
        removePendingEmail envDatabase userId
      pure (Just EmailTokenInitOutSuccess)
