{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}

{-|

Module: LocalCooking.Server.Dependencies.UserRoles
Copyright: (c) 2018 Local Cooking Inc.
License: Proprietary
Maintainer: athan.clark@localcooking.com
Portability: GHC

-}

module LocalCooking.Server.Dependencies.UserRoles where

import LocalCooking.Types (AppM)
import LocalCooking.Types.Env (Env (..))
import LocalCooking.Auth (usersAuthToken)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Role (UserRole)
import LocalCooking.Database.Query.User (getRoles)
import LocalCooking.Server.Dependencies.AccessToken.Generic (AuthInitIn (..), AuthInitOut (..))

import Web.Dependencies.Sparrow.Types (Server, JSONVoid, staticServer)

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), object, (.=), Value (Object, String))
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.JSONUnit (JSONUnit (..))
import Control.Applicative (Alternative)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)


type UserRolesInitIn = AuthInitIn AuthToken JSONUnit

type UserRolesInitOut = AuthInitOut [UserRole]


userRolesServer :: Alternative f
                => Server AppM f UserRolesInitIn
                                 UserRolesInitOut
                                 JSONVoid
                                 JSONVoid
userRolesServer = staticServer $ \(AuthInitIn authToken JSONUnit) -> do
  Env{envDatabase} <- ask

  mRole <- do
    mUserId <- usersAuthToken authToken
    case mUserId of
      Nothing -> pure Nothing
      Just userId -> Just <$> liftIO (getRoles envDatabase userId)

  case mRole of
    Nothing -> pure (Just AuthInitOutNoAuth)
    Just role -> pure $ Just $ AuthInitOut role
