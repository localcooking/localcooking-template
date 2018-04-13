{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , ScopedTypeVariables
  , DataKinds
  , RankNTypes
  , RecordWildCards
  #-}

module LocalCooking.Server where

import LocalCooking.Server.HTTP (httpServer)
import LocalCooking.Server.Dependencies (servedDependencies)
import LocalCooking.Types (AppM)
import LocalCooking.Types.Env (Env (..))
import LocalCooking.Links.Class (LocalCookingSiteLinks)
import LocalCooking.Colors (LocalCookingColors)

import Web.Routes.Nested (RouterT, textOnly)
import Web.Dependencies.Sparrow (SparrowServerT)
import Network.Wai.Handler.Warp (runEnv)
import Network.Wai.Trans (ApplicationT, MiddlewareT, runApplicationT)
import Network.HTTP.Types (status404)
import Data.Singleton.Class (runSingleton)
import Data.Time.Clock (secondsToDiffTime)
import qualified Data.TimeMap as TimeMap
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import Path.Extended (FromLocation, ToLocation)
import Control.Monad (void, forM_, forever)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control.Aligned (liftBaseWith)
import Control.Logging (log')
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TMapMVar.Hash as TMapMVar


data LocalCookingArgs siteLinks sec = LocalCookingArgs
  { localCookingArgsFrontend    :: BS.ByteString
  , localCookingArgsFrontendMin :: BS.ByteString
  , localCookingArgsFavicons    :: [(FilePath, BS.ByteString)]
  , localCookingArgsHTTP        :: (siteLinks -> MiddlewareT AppM)
                                -> RouterT (MiddlewareT AppM) sec AppM ()
  , localCookingArgsDeps        :: SparrowServerT (MiddlewareT AppM) AppM ()
  , localCookingArgsColors      :: LocalCookingColors
  }


server :: forall sec siteLinks
        . LocalCookingSiteLinks siteLinks
       => FromLocation siteLinks
       => ToLocation siteLinks
       => Int
       -> LocalCookingArgs siteLinks sec
       -> AppM ()
server port LocalCookingArgs{..} = do
  -- auth token expiring checker - FIXME use a cassandra database instead probably
  env@Env{envAuthTokens,envAuthTokenExpire} <- ask
  liftIO $ void $ async $ forever $ do
    xs <- flip TimeMap.takeFromNow envAuthTokens $ fromRational $ toRational $ secondsToDiffTime $
      let minute = 60
          hour = 60 * minute
          day = 24 * hour
      in  2 * minute -- FIXME expire after a day?
    forM_ xs $ \(authToken,userId) -> do
      log' $ "Auth token revoked: " <> T.pack (show authToken) <> ", for " <> T.pack (show userId)
      atomically (TMapMVar.insert envAuthTokenExpire authToken ())
    threadDelay $
      let second = 10 ^ 6
          minute = second * 60
      in  minute

  -- HTTP Server
  liftBaseWith $ \runInBase -> do
    dependencies <- runSingleton <$> runInBase (servedDependencies localCookingArgsDeps)
    server' <- fmap runSingleton $ runInBase $ runApplicationT $
      httpServer
        localCookingArgsFrontend
        localCookingArgsFrontendMin
        localCookingArgsFavicons
        localCookingArgsColors
        (Proxy :: Proxy siteLinks)
        localCookingArgsHTTP
        dependencies
        defApp
    runEnv port server'


defApp :: ApplicationT AppM
defApp _ respond = respond $ textOnly "404" status404 []
