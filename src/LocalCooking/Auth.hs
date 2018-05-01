{-# LANGUAGE
    NamedFieldPuns
  #-}

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



loginAuth :: UserId -> AppM AuthToken
loginAuth userId = do
  Env{envTokenContexts = TokenContexts{tokenContextAuth}} <- ask

  liftIO (insertAccess tokenContextAuth userId)


usersAuthToken :: AuthToken -> AppM (Maybe UserId)
usersAuthToken authToken = do
  Env{envTokenContexts = TokenContexts{tokenContextAuth}} <- ask

  liftIO (lookupAccess tokenContextAuth authToken)


logoutAuth :: AuthToken -> AppM ()
logoutAuth authToken = do
  Env{envTokenContexts = TokenContexts{tokenContextAuth}} <- ask

  liftIO $ atomically $ revokeAccess tokenContextAuth authToken
