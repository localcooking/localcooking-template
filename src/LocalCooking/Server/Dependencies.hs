{-# LANGUAGE
    OverloadedStrings
  #-}

module LocalCooking.Server.Dependencies where

import LocalCooking.Server.Dependencies.AuthToken (authTokenServer)
import LocalCooking.Server.Dependencies.Register (registerServer)
import LocalCooking.Types (AppM)

import Web.Routes.Nested (RouterT, l_, o_, (</>))
import Web.Dependencies.Sparrow (Topic (..), serveDependencies, unpackServer, SparrowServerT, match)
import Network.Wai.Trans (MiddlewareT)


dependencies :: SparrowServerT (MiddlewareT AppM) AppM () -- ^ Extra deps
             -> SparrowServerT (MiddlewareT AppM) AppM ()
dependencies deps = do
  match (l_ "authToken" </> o_) =<< unpackServer (Topic ["authToken"]) authTokenServer
  match (l_ "register" </> o_) =<< unpackServer (Topic ["register"]) registerServer
  deps


servedDependencies :: SparrowServerT (MiddlewareT AppM) AppM ()
                   -> AppM (RouterT (MiddlewareT AppM) sec AppM ())
servedDependencies = serveDependencies . dependencies
