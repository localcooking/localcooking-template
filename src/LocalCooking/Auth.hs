{-# LANGUAGE
    NamedFieldPuns
  #-}

module LocalCooking.Auth where

import LocalCooking.Types (AppM)
import LocalCooking.Types.Env (Env (..))
import LocalCooking.Common.AuthToken (AuthToken, genAuthToken)
import LocalCooking.Database.Schema.User (UserId)

import qualified Data.TimeMap as TimeMap
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TMapMVar.Hash as TMapMVar



loginAuth :: UserId -> AppM AuthToken
loginAuth userId = do
  Env{envAuthTokens} <- ask

  liftIO $ do
    authToken <- genAuthToken
    TimeMap.insert authToken userId envAuthTokens
    pure authToken


usersAuthToken :: AuthToken -> AppM (Maybe UserId)
usersAuthToken authToken = do
  Env{envAuthTokens} <- ask

  liftIO $ do
    mUserId <- atomically (TimeMap.lookup authToken envAuthTokens)
    TimeMap.touch authToken envAuthTokens
    pure mUserId


logoutAuth :: AuthToken -> AppM ()
logoutAuth authToken = do
  Env{envAuthTokens,envAuthTokenExpire} <- ask

  liftIO $ atomically $ do
    TimeMap.delete authToken envAuthTokens
    TMapMVar.insert envAuthTokenExpire authToken ()
