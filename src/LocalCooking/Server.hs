{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , ScopedTypeVariables
  , DataKinds
  , RankNTypes
  , RecordWildCards
  , FlexibleContexts
  #-}

{-|

Module: LocalCooking.Server
Copyright: (c) 2018 Local Cooking Inc.
License: Proprietary
Maintainer: athan.clark@localcooking.com
Portability: GHC

-}

module LocalCooking.Server where

import LocalCooking.Server.HTTP (httpServer)
import LocalCooking.Server.Dependencies (dependencies)
import LocalCooking.Types (AppM)
import LocalCooking.Types.Env (Env (..), TokenContexts (..))
import LocalCooking.Links.Class (LocalCookingSiteLinks)
import LocalCooking.Colors (LocalCookingColors)
-- import LocalCooking.Server.Dependencies.AccessToken.Generic (expireThread)

import Web.Routes.Nested (RouterT, textOnly)
import Web.Dependencies.Sparrow (SparrowServerT, serveDependencies)
import Network.Wai.Handler.Warp (runEnv)
import Network.Wai.Trans (ApplicationT, MiddlewareT, runApplicationT)
import Network.HTTP.Types (status404)
import Data.Singleton.Class (runSingleton)
import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))
import Data.Insert.Class (Insertable)
import Path.Extended (FromLocation, ToLocation)
import Control.Applicative (Alternative)
import Control.Monad (void)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control.Aligned (liftBaseWith)
import Control.Concurrent.Async (async)


-- | Top-level Local Cooking server template arguments
data LocalCookingArgs siteDeps siteLinks sec f = LocalCookingArgs
  { localCookingArgsFrontend    :: BS.ByteString -- ^ Raw frontend javascript
  , localCookingArgsFrontendMin :: BS.ByteString -- ^ Raw minified frontend javascript
  , localCookingArgsFavicons    :: [(FilePath, BS.ByteString)] -- ^ Favicon directory asset contents
  , localCookingArgsHTTP        :: (siteLinks -> MiddlewareT AppM)
                                -> RouterT (MiddlewareT AppM) sec AppM () -- ^ Casual HTTP links
  , localCookingArgsDeps        :: SparrowServerT (MiddlewareT AppM) f AppM () -- ^ Casual Sparrow dependencies
  , localCookingArgsColors      :: LocalCookingColors -- ^ Site-wide colors
  }


-- | Majority of business logic
server :: forall sec siteLinks f
        . LocalCookingSiteLinks siteLinks
       => FromLocation siteLinks
       => ToLocation siteLinks
       => Alternative f
       => Insertable f AppM
       => Foldable f
       => Int -- ^ Port to bind to
       -> LocalCookingArgs siteLinks sec f
       -> AppM ()
server port LocalCookingArgs{..} = do
  -- auth token expiring checker - FIXME use a cassandra database instead probably
  -- Env{envTokenContexts = TokenContexts{tokenContextAuth}} <- ask
  -- liftIO $ void $ async $ -- forever $ do
  --   let delay =
  --         let second = 10 ^ 6
  --             minute = second * 60
  --         in  minute
  --   in  expireThread delay tokenContextAuth

  -- HTTP Server
  liftBaseWith $ \runInBase -> do
    ds <- runSingleton <$> runInBase (serveDependencies (dependencies localCookingArgsDeps))
    server' <- fmap runSingleton $ runInBase $ runApplicationT $
      httpServer
        localCookingArgsFrontend
        localCookingArgsFrontendMin
        localCookingArgsFavicons
        localCookingArgsColors
        (Proxy :: Proxy siteLinks)
        localCookingArgsHTTP
        ds
        defApp
    runEnv port server'


-- | Simple @404@ response
defApp :: ApplicationT AppM
defApp _ respond = respond $ textOnly "404" status404 []
