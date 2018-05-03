{-# LANGUAGE
    NamedFieldPuns
  #-}

{-|

Module: LocalCooking.Auth
Copyright: (c) 2018 Local Cooking Inc.
License: Proprietary
Maintainer: athan.clark@localcooking.com
Portability: GHC

Convenience functions for working with 'LocalCooking.Common.AccessToken.Auth.AuthToken's, within
a 'LocalCooking.Types.AppM' environment.

-}

module LocalCooking.Auth where

import LocalCooking.Types (AppM)
import LocalCooking.Types.Env (Env (..), TokenContexts (..))
import LocalCooking.Common.AccessToken (genAccessToken)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Server.Dependencies.AccessToken.Generic (lookupAccess, insertAccess, revokeAccess)
import LocalCooking.Database.Schema.User (UserId)

import qualified Data.TimeMap as TimeMap
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TMapMVar.Hash as TMapMVar
import Control.Newtype (Newtype (pack))


-- | Wraps 'LocalCooking.Server.Dependencies.AccessToken.Generic.insertAccess'.
loginAuth :: UserId -> AppM AuthToken
loginAuth userId = do
  Env{envTokenContexts = TokenContexts{tokenContextAuth}} <- ask
  liftIO (insertAccess tokenContextAuth userId)


-- | Wraps 'LocalCooking.Server.Dependencies.AccessToken.Generic.lookupAccess'.
usersAuthToken :: AuthToken -> AppM (Maybe UserId)
usersAuthToken authToken = do
  Env{envTokenContexts = TokenContexts{tokenContextAuth}} <- ask
  liftIO (lookupAccess tokenContextAuth authToken)


-- | Wraps 'LocalCooking.Server.Dependencies.AccessToken.Generic.revokeAccess'.
logoutAuth :: AuthToken -> AppM ()
logoutAuth authToken = do
  Env{envTokenContexts = TokenContexts{tokenContextAuth}} <- ask
  liftIO $ atomically $ revokeAccess tokenContextAuth authToken
