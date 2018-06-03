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
import LocalCooking.Dependencies (dependencies)
import LocalCooking.Types (Env)
import LocalCooking.Function.System (SystemM)
-- import LocalCooking.Types.Env (Env (..), TokenContexts (..))
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
-- import Data.Insert.Class (Insertable)
import Path.Extended (FromLocation, ToLocation)
import Control.Applicative (Alternative)
import Control.Monad (void)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control.Aligned (liftBaseWith)
import Control.Concurrent.Async (async)


-- | Top-level Local Cooking server template arguments
data LocalCookingArgs siteLinks sec = LocalCookingArgs
  { localCookingArgsFrontend    :: BS.ByteString -- ^ Raw frontend javascript
  , localCookingArgsFrontendMin :: BS.ByteString -- ^ Raw minified frontend javascript
  , localCookingArgsFavicons    :: [(FilePath, BS.ByteString)] -- ^ Favicon directory asset contents
  , localCookingArgsHTTP        :: (siteLinks -> MiddlewareT SystemM)
                                -> RouterT (MiddlewareT SystemM) sec SystemM () -- ^ Casual HTTP links
  , localCookingArgsDeps        :: SparrowServerT (MiddlewareT SystemM) [] SystemM () -- ^ Casual Sparrow dependencies
  , localCookingArgsColors      :: LocalCookingColors -- ^ Site-wide colors
  }


-- | Majority of business logic
server :: forall sec siteLinks
        . LocalCookingSiteLinks siteLinks
       => FromLocation siteLinks
       => ToLocation siteLinks
       => Env
       -> Int -- ^ Port to bind to
       -> LocalCookingArgs siteLinks sec
       -> SystemM ()
server env port LocalCookingArgs{..} = do
  -- HTTP Server
  liftBaseWith $ \runInBase -> do
    ds <- runSingleton <$> runInBase (serveDependencies (dependencies localCookingArgsDeps))
    server' <- fmap runSingleton $ runInBase $ runApplicationT $
      httpServer
        env
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
defApp :: ApplicationT SystemM
defApp _ respond = respond (textOnly "404" status404 [])
