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
import Data.Aeson.JSONUnit (JSONUnit (..))
import Control.Applicative (Alternative)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)


type UserEmailInitIn = AuthInitIn AuthToken JSONUnit

type UserEmailInitOut = AuthInitOut EmailAddress


userEmailServer :: Alternative f
                => Server AppM f UserEmailInitIn
                                 UserEmailInitOut
                                 JSONVoid
                                 JSONVoid
userEmailServer = staticServer $ \(AuthInitIn authToken JSONUnit) -> do
  Env{envDatabase} <- ask

  mEmail <- do
    mUserId <- usersAuthToken authToken
    case mUserId of
      Nothing -> pure Nothing
      Just userId -> liftIO (getEmail envDatabase userId)

  case mEmail of
    Nothing -> pure (Just AuthInitOutNoAuth)
    Just email -> pure $ Just $ AuthInitOut email
