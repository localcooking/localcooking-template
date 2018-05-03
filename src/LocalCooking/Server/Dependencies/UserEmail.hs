{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}

{-|

Module: LocalCooking.Server.Dependencies.UserEmail
Copyright: (c) 2018 Local Cooking Inc.
License: Proprietary
Maintainer: athan.clark@localcooking.com
Portability: GHC

-}

module LocalCooking.Server.Dependencies.UserEmail where

import LocalCooking.Types (AppM)
import LocalCooking.Types.Env (Env (..))
import LocalCooking.Auth (usersAuthToken)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Database.Query.User (getEmail)
import LocalCooking.Server.Dependencies.AccessToken.Generic (AuthInitIn (..), AuthInitOut (..))

import Web.Dependencies.Sparrow.Types (Server, JSONVoid, staticServer)

import Text.EmailAddress (EmailAddress)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), object, (.=), Value (Object, String))
import Data.Aeson.Types (typeMismatch)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)


type UserEmailInitIn = AuthInitIn AuthToken ()
-- newtype UserEmailInitIn = UserEmailInitIn
--   { userEmailInitInAuthToken :: AuthToken
--   }

-- instance FromJSON UserEmailInitIn where
--   parseJSON json = case json of
--     Object o -> UserEmailInitIn <$> o .: "authToken"
--     _ -> fail
--     where
--       fail = typeMismatch "UserEmailInitIn" json


type UserEmailInitOut = AuthInitOut EmailAddress
-- data UserEmailInitOut
--   = UserEmailInitOutNoAuth
--   | UserEmailInitOutSuccess EmailAddress

-- instance ToJSON UserEmailInitOut where
--   toJSON x = case x of
--     UserEmailInitOutNoAuth -> String "no-auth"
--     UserEmailInitOutSuccess y -> object ["email" .= y]


userEmailServer :: Server AppM UserEmailInitIn
                               UserEmailInitOut
                               JSONVoid
                               JSONVoid
userEmailServer = staticServer $ \(AuthInitIn authToken ()) -> do
  Env{envDatabase} <- ask

  mEmail <- do
    mUserId <- usersAuthToken authToken
    case mUserId of
      Nothing -> pure Nothing
      Just userId -> liftIO $ getEmail envDatabase userId

  case mEmail of
    Nothing -> pure $ Just AuthInitOutNoAuth
    Just email -> pure $ Just $ AuthInitOut email
